(* variable/function/contract identifier *)
type ide = string

(* token identifier *)
type tok = string

(* address identifier *)
type addr = string

(* expressions *)

type expr =
  | True
  | False
  | Var of ide
  | IntConst of int
  | AddrConst of addr
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr
  | Le of expr * expr           
  | Geq of expr * expr
  | Ge of expr * expr           

(* commands *)
          
and cmd =
  | Skip
  | Assign of ide * expr
  | Seq of cmd * cmd
  | Send of ide * expr * tok (* send(e1,e2,t) transfers e2:t to e1 *)
  | If of expr * cmd * cmd
  | Req of expr               (* require(e) reverts if e is false *) 
  | Call of ide * expr        (* TODO: add actual parameters *)
  | CallExec of cmd           (* Runtime only: c is the cmd being reduced *)
  | Block of var_decls * cmd
  | ExecBlock of cmd          (* Runtime only: c is the cmd being reduced *)

and arg =
  | IntArg of ide
  | RcvArg of ide * tok

and args = arg list

and var_decl =
  | IntVar of ide 
  | BoolVar of ide 
  | AddrVar of ide 

and fun_decl =
  | Constr of ide * args * cmd
  | Proc of ide * args * cmd

and var_decls = var_decl list

and fun_decls = fun_decl list

type contract = Contract of ide * var_decls * fun_decls


(* tx = sender:contract.function(args) *)
type tx = Tx of addr * ide * ide * args
