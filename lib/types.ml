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

(* a stack of (storage,env) frames *)
type state = (storage * env) list

type exec_state = St of state | Cmd of cmd * state * addr

let topframe (st: state) : storage * env = match st with
    [] -> failwith "empty stack"
  | h::_ -> h

let popframe (st: state) : state = match st with
    [] -> failwith "empty stack"
  | _::st' -> st'

let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
let botstorage = fun a -> failwith ("address " ^ a ^ " unbound")
    
let bind f x v = fun y -> if y=x then v else f y

(* lookup for variable x in state st (tries first in storage of address a) *)
    
let lookup (st:state) (a:addr) (x:ide) : exprval =
  let (s,r) = topframe st in
  try s a x      (* look up for x in storage of a *)
  with _ -> r x  (* look up for x in environment *)

let mem_storage (s:storage) (a:addr) (x:ide) : bool = 
  try let _ = s a x in true
  with _ -> false

let mem_env (r:env) (x:ide) : bool =
  try let _= r x in true
  with _ -> false

let update_storage (st:state) (a:addr) (x:ide) (v:exprval) : state = match st with
  [] -> failwith "Empty stack"
| (s,r)::st' -> 
    if mem_storage s a x then 
      let sa' = bind (s a) x v in
      let s' = bind s a sa' in 
      (s',r)::st'
    else failwith (x ^ " not bound in storage of " ^ a)   


let update_env (st:state) (x:ide) (v:exprval) : state = match st with
  [] -> failwith "Empty stack"
| (s,r)::st' -> 
    if mem_env r x then 
      let r' = bind r x v in 
      (s,r')::st'
    else failwith (x ^ " not bound in env")   

exception TypeError of string
exception UnboundVar of ide
