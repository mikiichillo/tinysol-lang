open Ast

type exprval = Bool of bool | Int of int | Addr of string

(* TODO: not used yet *)
type envval  = IVar of exprval | IProc of args * cmd

(* environment: 
     maps local variables, plus sender and value
     this is a transient storage, not preserved between calls
*)
type env = ide -> exprval

(* storage:
     maps addresses to environments
     this is a persistent storage, preserved between calls
*)
type storage = addr -> env

type sysstate = storage * (env list)

let get_storage (st : sysstate) = fst st
let get_envstack (st : sysstate) = snd st


type exec_state = St of sysstate | Cmd of cmd * sysstate * addr

let topenv (st: sysstate) : env = match get_envstack st with
  [] -> failwith "empty stack"
| e::_ -> e

let popenv (st: sysstate) : env list = match get_envstack st with
    [] -> failwith "empty stack"
  | _::el -> el

let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
let botstorage = fun a -> failwith ("address " ^ a ^ " unbound")
    
let bind f x v = fun y -> if y=x then v else f y

(* lookup for variable x in state st (tries first in storage of address a) *)
    
let lookup (st : sysstate) (a : addr) (x : ide) : exprval =
  try 
    let e = topenv st in
    e x                     (* look up for x in environment *)
  with _ -> 
    (get_storage st) a x    (* look up for x in storage of a *)

let mem_storage (s:storage) (a:addr) (x:ide) : bool = 
  try let _ = s a x in true
  with _ -> false

let mem_env (r:env) (x:ide) : bool =
  try let _= r x in true
  with _ -> false

let update_storage (st : sysstate) (a:addr) (x:ide) (v:exprval) : sysstate = 
  let (s,el) = get_storage st, get_envstack st in
    if mem_storage s a x then 
      let sa' = bind (s a) x v in
      let s' = bind s a sa' in 
      (s', el)
    else failwith (x ^ " not bound in storage of " ^ a)   

let update_env (st : sysstate) (x:ide) (v:exprval) : sysstate =
  let (s,el) = get_storage st, get_envstack st in match el with
  | [] -> failwith "Empty stack"
  | e::el' -> 
    if mem_env e x then 
      let e' = bind e x v in 
      (s, e'::el')
    else failwith (x ^ " not bound in env")   

exception TypeError of string
exception UnboundVar of ide
