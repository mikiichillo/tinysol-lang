open Ast
open Types
open Utils


let eval_var_decls (vdl : var_decl list) (e : env): env =
  List.fold_left
    (fun acc vd ->
      match vd with
        | VarT(IntBT,_),x  
        | VarT(UintBT,_),x -> acc |> bind x (Int 0)
        | VarT(BoolBT,_),x -> acc |> bind x (Bool false)
        | VarT(AddrBT _,_),x -> acc |> bind x (Addr "0")
        | VarT(CustomBT _,_),x -> acc |> bind x (Int 0)
        | MapT(_),_ -> failwith "mappings cannot be used in local declarations" 
    )
    e
    vdl


let rec gather_decls = function
  | Decl d -> [d]
  | Seq(c1,c2) -> gather_decls c1 @ gather_decls c2
  | _ -> []

let rec purge_decls = function
  | Decl _ -> Skip
  | Seq(Decl _,c2) -> purge_decls c2
  | Seq(c1,Decl _) -> purge_decls c1
  | Seq(c1,c2) -> Seq(purge_decls c1, purge_decls c2)
  | Block(vdl,c) -> Block(vdl @ gather_decls c, purge_decls c)
  | _ as c -> c 

let rec blockify_cmd c = 
  let vdl = gather_decls c in
  let c' = purge_decls c in
  if vdl=[] then blockify_subterms c'
  else Block(vdl, blockify_subterms c')

and blockify_subterms = function
  | Block(vdl,c) -> Block(vdl, blockify_subterms c) 
  | Seq(c1,c2) -> Seq(blockify_subterms c1, blockify_subterms c2) 
  | If(e,c1,c2) -> If(e, blockify_cmd c1, blockify_cmd c2)
  | _ as c -> c

let blockify_fun = function
  | Constr (al,c,p) -> Constr (al,blockify_cmd c,p)
  | Proc (f,al,c,v,p,r) -> Proc(f,al,blockify_cmd c,v,p,r)

let blockify_contract (Contract(c,el,vdl,fdl)) =
  Contract(c,el,vdl,List.map blockify_fun fdl)


(******************************************************************************)
(*              Retrieving contracts and functions from state                 *)
(******************************************************************************)

let find_fun_in_contract (Contract(_,_,_,fdl)) (f : ide) : fun_decl option =
  List.fold_left 
  (fun acc fd -> match fd with
    | Constr(_) -> if acc=None && f="constructor" then Some fd else acc  
    | Proc(g,_,_,_,_,_) -> if acc=None && f=g then Some fd else acc
  )
  None
  fdl

let find_fun_in_sysstate (st : sysstate) (a : addr) (f : ide) = 
  if not (exists_account st a) then
    failwith ("address " ^ a ^ " does not exist")
  else match (st.accounts a).code with
    | None -> None  (* "address " ^ a ^ " is not a contract address" *)
    | Some(c) -> find_fun_in_contract c f 

let get_cmd_from_fun = function
  | (Constr(_,c,_)) -> c
  | (Proc(_,_,c,_,_,_)) -> c

let get_var_decls_from_fun = function
  | (Constr(vdl,_,_)) -> vdl
  | (Proc(_,vdl,_,_,_,_)) -> vdl

let bind_fargs_aargs (xl : var_decl list) (vl : exprval list) : env =
  if List.length xl <> List.length vl then
    failwith "exec_tx: length mismatch between formal and actual arguments"
  else 
  List.fold_left2 
  (fun acc x_decl v -> match (x_decl,v) with 
   | ((VarT(IntBT,_),x), Int _)
   | ((VarT(BoolBT,_),x), Bool _) 
   | ((VarT(AddrBT _,_),x), Addr _) -> bind x v acc
   | ((VarT(UintBT,_),x), Int n) when n>=0 -> bind x v acc
   | ((MapT(_),_),_) -> failwith "Maps cannot be passed as function parameters"
   | _ -> failwith "exec_tx: type mismatch between formal and actual arguments") 
  botenv 
  xl 
  vl 

(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)

exception TypeError of string
exception NoRuleApplies

let is_val = function
  | BoolConst _ 
  | IntConst _
  | AddrConst _ -> true
  | _ -> false

let eval_expr0 = function
  | BoolConst b -> (Bool b)
  | IntConst n  -> (Int n)
  | AddrConst s -> (Addr s)
  | e -> failwith (string_of_expr e ^ " is not a value")

let expr0_of_exprval = function
  | Bool b -> BoolConst b
  | Int n -> IntConst n
  | Addr b -> AddrConst b
  | Map _ -> failwith "step_expr: wrong type checking of map?"

(* Il risultato di Var x deve diventare una espressione costante!! (idem per le mappe) *)

let int_of_exprval v = match v with 
  | Int n  -> n
  | Bool _ -> failwith "value has type Bool but an Int was expected"
  | Addr _ -> failwith "value has type Addr but an Int was expected"
  | Map _  -> failwith "value has type Map but an Int was expected"

let bool_of_exprval v = match v with 
  | Bool b -> b
  | Int _  -> failwith "value has type Int but an Bool was expected"
  | Addr _ -> failwith "value has type Addr but an Bool was expected"
  | Map _  -> failwith "value has type Map but an Bool was expected"

let int_of_expr e = match e with 
  | IntConst n  -> n
  | _ -> failwith "IntConst was expected"

let bool_of_expr e = match e with 
  | BoolConst b -> b
  | _  -> failwith "True or False was expected"

let addr_of_expr e = match e with 
  | AddrConst a -> a
  | _ -> failwith "AddrConst was expected"

let addr_of_exprval v = match v with 
  | Addr a -> a
  | Bool _ -> failwith "value has type Bool but an Addr was expected"
  | Int _ -> failwith "value has type Int but an Addr was expected"
  | Map _ -> failwith "value has type Map but an Addr was expected"

let rec step_expr (e,st,a) = match e with
  | e when is_val e -> raise NoRuleApplies

  | This -> (AddrConst a, st, a)

  | BlockNum -> (IntConst st.blocknum, st, a)

  | Var x -> (expr0_of_exprval (lookup_var a x st), st, a)  

  | MapR(Var x,e2) when is_val e2 -> (match lookup_var a x st with
    | Map m -> (expr0_of_exprval (m (eval_expr0 e2)), st, a)
    | _ -> failwith "step_expr: wrong type checking of map?")
  | MapR(Var x,e2) ->
    let (e2', st', a') = step_expr (e2, st, a) in (MapR(Var x,e2'), st', a')
  | MapR(e1,e2) ->
    let (e1', st', a') = step_expr (e1, st, a) in (MapR(e1',e2), st', a')

  | BalanceOf e when is_val e -> 
    let b = addr_of_expr e in (IntConst (lookup_balance b st), st, a)
  | BalanceOf e -> 
    let (e', st', a') = step_expr (e, st, a) in (BalanceOf e', st', a')

  | Not(e) when is_val e -> 
    let b = bool_of_expr e in (BoolConst (not b), st, a)
  | Not(e) -> 
    let (e', st', a') = step_expr (e, st, a) in (Not e', st', a')

  | And(e1,e2) when is_val e1 && is_val e2 ->
    let (b1,b2) = bool_of_expr e1,bool_of_expr e2 in 
    (BoolConst (b1 && b2), st, a)         
  | And(e1,e2) when is_val e1 ->
    let (e2', st', a') = step_expr (e2, st, a) in (And(e1,e2'), st', a')
  | And(e1,e2) -> 
    let (e1', st', a') = step_expr (e1, st, a) in (And(e1',e2), st', a')

  | Or(e1,e2) when is_val e1 && is_val e2 ->
    let (b1,b2) = bool_of_expr e1,bool_of_expr e2 in 
    (BoolConst(b1 || b2), st, a)         
  | Or(e1,e2) when is_val e1 ->
    let (e2', st', a') = step_expr (e2, st, a) in (Or(e1,e2'), st', a')
  | Or(e1,e2) -> 
    let (e1', st', a') = step_expr (e1, st, a) in (Or(e1',e2), st', a')

  | Add(e1,e2) when is_val e1 && is_val e2 ->
    let (n1,n2) = int_of_expr e1,int_of_expr e2 in 
    (IntConst (n1+n2), st, a)         
  | Add(e1,e2) when is_val e1 ->
    let (e2', st', a') = step_expr (e2, st, a) in (Add(e1,e2'), st', a')
  | Add(e1,e2) -> 
    let (e1', st', a') = step_expr (e1, st, a) in (Add(e1',e2), st', a')

  | Sub(e1,e2) when is_val e1 && is_val e2 ->
    let (n1,n2) = int_of_expr e1,int_of_expr e2 in 
    (IntConst (n1+n2), st, a)         
  | Sub(e1,e2) when is_val e1 ->
    let (e2', st', a') = step_expr (e2, st, a) in (Sub(e1,e2'), st', a')
  | Sub(e1,e2) -> 
    let (e1', st', a') = step_expr (e1, st, a) in (Sub(e1',e2), st', a')

  | Mul(e1,e2) when is_val e1 && is_val e2 ->
    let (n1,n2) = int_of_expr e1,int_of_expr e2 in 
    (IntConst (n1*n2), st, a)         
  | Mul(e1,e2) when is_val e1 ->
    let (e2', st', a') = step_expr (e2, st, a) in (Mul(e1,e2'), st', a')
  | Mul(e1,e2) -> 
    let (e1', st', a') = step_expr (e1, st, a) in (Mul(e1',e2), st', a')

  | Eq(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
      | (IntConst n1,IntConst n2) -> (BoolConst(n1=n2), st, a)
      | (AddrConst a1,AddrConst a2) -> (BoolConst(a1=a2), st, a)
      | (BoolConst b1,BoolConst b2) -> (BoolConst(b1=b2), st, a)
      | _ -> raise (TypeError "Eq"))
  | Eq(e1,e2) when is_val e1 ->
    let (e2', st', a') = step_expr (e2, st, a) in (Eq(e1,e2'), st', a')
  | Eq(e1,e2) -> 
    let (e1', st', a') = step_expr (e1, st, a) in (Eq(e1',e2), st', a')

  | Neq(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
      | (IntConst n1,IntConst n2) -> (BoolConst(n1<>n2), st, a)
      | (AddrConst a1,AddrConst a2) -> (BoolConst(a1<>a2), st, a)
      | (BoolConst b1,BoolConst b2) -> (BoolConst(b1<>b2), st, a)
      | _ -> raise (TypeError "Neq"))
  | Neq(e1,e2) when is_val e1 ->
    let (e2', st', a') = step_expr (e2, st, a) in (Neq(e1,e2'), st', a')
  | Neq(e1,e2) -> 
    let (e1', st', a') = step_expr (e1, st, a) in (Neq(e1',e2), st', a')

  | Leq(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
      | (IntConst n1,IntConst n2) -> (BoolConst(n1<=n2), st, a)
      | _ -> raise (TypeError "Leq"))
  | Leq(e1,e2) when is_val e1 ->
    let (e2', st', a') = step_expr (e2, st, a) in (Leq(e1,e2'), st', a')
  | Leq(e1,e2) -> 
    let (e1', st', a') = step_expr (e1, st, a) in (Leq(e1',e2), st', a')

  | Lt(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
      | (IntConst n1,IntConst n2) -> (BoolConst(n1<n2), st, a)
      | _ -> raise (TypeError "Leq"))
  | Lt(e1,e2) when is_val e1 ->
    let (e2', st', a') = step_expr (e2, st, a) in (Lt(e1,e2'), st', a')
  | Lt(e1,e2) -> 
    let (e1', st', a') = step_expr (e1, st, a) in (Lt(e1',e2), st', a')

  | Geq(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
      | (IntConst n1,IntConst n2) -> (BoolConst(n1>=n2), st, a)
      | _ -> raise (TypeError "Leq"))
  | Geq(e1,e2) when is_val e1 ->
    let (e2', st', a') = step_expr (e2, st, a) in (Geq(e1,e2'), st', a')
  | Geq(e1,e2) -> 
    let (e1', st', a') = step_expr (e1, st, a) in (Geq(e1',e2), st', a')

  | Gt(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
      | (IntConst n1,IntConst n2) -> (BoolConst(n1>n2), st, a)
      | _ -> raise (TypeError "Leq"))
  | Gt(e1,e2) when is_val e1 ->
    let (e2', st', a') = step_expr (e2, st, a) in (Gt(e1,e2'), st', a')
  | Gt(e1,e2) -> 
    let (e1', st', a') = step_expr (e1, st, a) in (Gt(e1',e2), st', a')

  | IfE(e1,e2,e3) when is_val e1 -> 
    let b1 = bool_of_expr e1 in ((if b1 then e2 else e3), st, a)
  | IfE(e1,e2,e3) -> 
    let (e1', st', a') = step_expr (e1, st, a) in (IfE(e1',e2,e3), st', a')    

  | IntCast(e) when is_val e -> 
      let n = int_of_expr e in (IntConst n, st, a)
  | IntCast(e) -> 
    let (e', st', a') = step_expr (e, st, a) in (IntCast(e'), st', a')    

  | UintCast(e) when is_val e -> 
      let n = int_of_expr e in (IntConst n, st, a)
  | UintCast(e) -> 
    let (e', st', a') = step_expr (e, st, a) in (UintCast(e'), st', a')    

  | AddrCast(e) when is_val e -> 
      let b = addr_of_expr e in (AddrConst b, st, a)
  | AddrCast(e) -> 
    let (e', st', a') = step_expr (e, st, a) in (AddrCast(e'), st', a')    

  | PayableCast(e) when is_val e -> 
      let b = addr_of_expr e in (AddrConst b, st, a) (* payable cast is only implemented by the type checker *)
  | PayableCast(e) -> 
    let (e', st', a') = step_expr (e, st, a) in (PayableCast(e'), st', a')    

  | EnumOpt(x,o) -> (match lookup_enum_option st a x o with
    | Some n -> (IntConst n, st, a)
    | None -> failwith "Enum lookup failed (bug in typechecking?)")

  | EnumCast(x,e) when is_val e -> (match eval_expr0 e with
      | Int n -> (match reverse_lookup_enum_option st a x n with
        | Some _ -> (IntConst n, st, a)
        | None -> raise (TypeError "EnumCast"))
      | _ -> raise (TypeError "EnumCast: expression is not an Int")
      )

  | EnumCast(x,e) -> 
    let (e', st', a') = step_expr (e, st, a) in (EnumCast(x,e'), st', a')    

  | FunCall(e_to,f,e_value,e_args) when is_val e_to && is_val e_value && List.for_all is_val e_args ->  
    (* retrieve function declaration *)
    let _ = print_endline("1111111111111111111111111111111111111") in
    let txto = addr_of_expr e_to in
    let txvalue  = int_of_expr e_value in
    let txargs = List.map (fun arg -> eval_expr0 arg) e_args in
    if lookup_balance a st < txvalue then 
      failwith ("sender " ^ a ^ " has not sufficient wei balance")
    else
    let sender_state = 
      { (st.accounts a) with balance = (st.accounts a).balance - txvalue } in
    let to_state  = 
      { (st.accounts txto) with balance = (st.accounts txto).balance + txvalue } in 
    let fdecl = Option.get (find_fun_in_sysstate st txto f) in  
    (* setup new stack frame TODO *)
    let xl = get_var_decls_from_fun fdecl in
    let xl',vl' =
      (VarT(AddrBT false,false),"msg.sender") :: (VarT(IntBT,false),"msg.value") :: xl,
      Addr a :: Int txvalue :: txargs
    in
    let e' = bind_fargs_aargs xl' vl' in
    let st' = { accounts = st.accounts 
                  |> bind a sender_state
                  |> bind txto to_state; 
                stackenv = e' :: st.stackenv;
                blocknum = st.blocknum;
                active = st.active } in
    let c = get_cmd_from_fun fdecl in
    let _ = print_endline ("FunCall 1  @" ^ a ^ " --> @" ^ txto ^ " : " ^ string_of_cmd c) in
    (ExecFunCall(c), st', txto)

  | FunCall(e_to,f,e_value,e_args) when is_val e_to && is_val e_value -> 
    let _ = print_endline("FunCall 2") in
    let (e_args', st', _) = step_expr_list (e_args, st, a) in 
    (FunCall(e_to,f,e_value,e_args'), st', a)

  | FunCall(e_to,f,e_value,e_args) when is_val e_to -> 
    let _ = print_endline("FunCall 3") in
    let (e_value', st', _) = step_expr (e_value, st, a) in 
    (FunCall(e_to,f,e_value',e_args), st', a)
  
  | FunCall(e_to,f,e_value,e_args) -> 
    let _ = print_string("FunCall 4: " ^ string_of_expr e_to ^ " ---> ") in
    let (e_to', st', _) = step_expr (e_to, st, a) in
    let _ = print_endline(string_of_expr e_to') in
    (FunCall(e_to',f,e_value,e_args), st', a)

  | ExecFunCall(Return e) when is_val e ->
    let _ = print_endline("ExecFunCall 1 @" ^ a ^ ": return " ^ string_of_expr e) in
    (e, popenv st, a)

  | ExecFunCall(c) -> (let _ = print_endline("ExecFunCall 2 @" ^ a ^ ": " ^ string_of_cmd c) in
    match step_cmd (Cmd(c,st,a)) with
    | St _ -> let _ = print_endline "ExecFunCall 2/St" in failwith "function terminated without return"
    | Reverted -> let _ = print_endline "ExecFunCall 2/Rev" in failwith "no"
    | Cmd(c',st',a') -> let _ = print_endline ("ExecFunCall 2/Cmd " ^ string_of_cmd c') in (ExecFunCall(c'),st',a'))

  | _ -> assert(false)

and step_expr_list (el,st,a) = match el with
  | [] -> (el, st, a)
  | e::tl when is_val e -> 
    let (el',st', a') = step_expr_list (tl, st, a) in (e::el', st', a')
  | e::tl -> 
    let (e',st', a') = step_expr (e, st, a) in (e'::tl, st', a')

(******************************************************************************)
(*                       Small-step semantics of commands                     *)
(******************************************************************************)

and step_cmd = function
    St _ -> raise NoRuleApplies
  | Reverted -> Reverted
  | Cmd(c,st,a) -> (match c with

    | Skip -> St st

    | Assign(x,e) when is_val e ->
        let _ = print_endline("Assign 1: " ^ a ^ "." ^ x ^ " = " ^ string_of_expr e) in
        St (update_var st a x (eval_expr0 e))
    | Assign(x,e) -> 
      let _ = print_endline("Assign 2: " ^ a ^ "." ^ x ^ " = " ^ string_of_expr e) in
      let (e', st', _) = step_expr (e, st, a) in 
      Cmd(Assign(x,e'), st', a)
      (* ERROR: e' should be evaluated in a', not in a *)

    | MapW(x,ek,ev) when is_val ek && is_val ev ->
        St (update_map st a x (eval_expr0 ek) (eval_expr0 ev))
    | MapW(x,ek,ev) when is_val ek -> 
      let (ev', st', _) = step_expr (ev, st, a) in 
      Cmd(MapW(x,ek,ev'), st', a)
    | MapW(x,ek,ev) -> 
      let (ek', st', _) = step_expr (ek, st, a) in 
      Cmd(MapW(x,ek',ev), st', a)
    
    | Seq(c1,c2) -> (match step_cmd (Cmd(c1,st,a)) with
        | St st1 -> Cmd(c2,st1,a)
        | Reverted -> Reverted
        | Cmd(c1',st1,a) -> Cmd(Seq(c1',c2),st1,a))

    | If(e,c1,c2) when is_val e -> (match eval_expr0 e with
          Bool true -> Cmd(c1,st,a)
        | Bool false -> Cmd(c2,st,a)
        | _ -> failwith("if: type error"))
    | If(e,c1,c2) -> 
        let (e', st', _) = step_expr (e, st, a) in
        Cmd(If(e',c1,c2), st', a)

    | Send(ercv,eamt) when is_val ercv && is_val eamt -> 
        let rcv = addr_of_expr ercv in 
        let amt = int_of_expr eamt in
        let bal = (st.accounts a).balance in
        if bal<amt then failwith "insufficient balance" else
        let sender_state =  { (st.accounts a) with balance = (st.accounts a).balance - amt } in
        if exists_account st rcv then
          let rcv_state = { (st.accounts rcv) with balance = (st.accounts rcv).balance + amt } in
           St { st with accounts = st.accounts |> bind rcv rcv_state |> bind a sender_state}
        else
          let rcv_state = { balance = amt; storage = botenv; code = None; } in
          St { st with accounts = st.accounts |> bind rcv rcv_state |> bind a sender_state; active = rcv::st.active }

    | Send(ercv,eamt) when is_val ercv -> 
        let (eamt', st', _) = step_expr (eamt, st, a) in
        Cmd(Send(ercv,eamt'), st', a)

    | Send(ercv,eamt) -> 
        let (ercv', st', _) = step_expr (ercv, st, a) in
        Cmd(Send(ercv',eamt), st', a)

    | Req(e) when is_val e -> 
        let b = bool_of_expr e in if b then St st else Reverted
    | Req(e) -> 
      let (e', st', _) = step_expr (e, st, a) in Cmd(Req(e'), st', a)

    | Return(e) when is_val e -> failwith "in questo caso dovrei terminare??"
    | Return(e) -> 
      let (e', st', _) = step_expr (e, st, a) in Cmd(Return(e'), st', a)
    
    | Block(vdl,c) ->
        let e' = eval_var_decls vdl botenv in
        Cmd(ExecBlock c, { st with stackenv = e'::st.stackenv} , a)

    | ExecBlock(c) -> (match step_cmd (Cmd(c,st,a)) with
        | St st -> St (popenv st)
        | Reverted -> Reverted
        | Cmd(c1',st1,a') -> Cmd(ExecBlock(c1'),st1,a'))

    | Decl _ -> assert(false) (* should not happen after blockify *)

    | ProcCall(e_to,f,e_value,e_args) when is_val e_to && is_val e_value && List.for_all is_val e_args ->
        (* retrieve function declaration *)
        let txto   = addr_of_expr e_to in
        let txvalue  = int_of_expr e_value in
        let txargs = List.map (fun arg -> eval_expr0 arg) e_args in
        if lookup_balance a st < txvalue then 
          failwith ("sender " ^ a ^ " has not sufficient wei balance")
        else
        let sender_state = 
          { (st.accounts a) with balance = (st.accounts a).balance - txvalue } in
        let to_state  = 
          { (st.accounts txto) with balance = (st.accounts txto).balance + txvalue } in 
        let fdecl = Option.get (find_fun_in_sysstate st txto f) in  
        (* setup new stack frame TODO *)
        let xl = get_var_decls_from_fun fdecl in
        let xl',vl' =
          (VarT(AddrBT false,false),"msg.sender") :: (VarT(IntBT,false),"msg.value") :: xl,
          Addr a :: Int txvalue :: txargs
        in
        let e' = bind_fargs_aargs xl' vl' in
        let st' = { accounts = st.accounts 
                      |> bind a sender_state
                      |> bind txto to_state; 
                    stackenv = e' :: st.stackenv;
                    blocknum = 0;
                    active = st.active } in
        let c = get_cmd_from_fun fdecl in
        Cmd(ExecBlock(c), st', txto)

    | ProcCall(e_to,f,e_value,e_args) when is_val e_to && is_val e_value -> 
      let (e_args', st', a') = step_expr_list (e_args, st, a) in 
      Cmd(ProcCall(e_to,f,e_value,e_args'), st', a')

    | ProcCall(e_to,f,e_value,e_args) when is_val e_to -> 
      let (e_value', st', a') = step_expr (e_value, st, a) in 
      Cmd(ProcCall(e_to,f,e_value',e_args), st', a')

    | ProcCall(e_to,f,e_value,e_args) -> 
      let (e_to', st', a') = step_expr (e_to, st, a) in 
      Cmd(ProcCall(e_to',f,e_value,e_args), st', a')

  )

(* recursively evaluate expression until it reaches a value (might not terminate) *)
let rec eval_expr (a : addr) (st : sysstate) (e : expr) : exprval = 
  if is_val e then eval_expr0 e
  else let (e', st' , a') = step_expr (e, st, a) in eval_expr a' st' e'  

let default_value = function 
  IntBT  
| UintBT -> Int 0
| BoolBT -> Bool false
| AddrBT _ -> Addr "0"
| CustomBT _ -> Int 0

let init_storage (Contract(_,_,vdl,_)) : ide -> exprval =
  List.fold_left (fun acc vd -> 
      let (x,v) = (match vd with 
        | VarT(t,_),x -> (x, default_value t)
        | MapT(_,tv),x -> (x, Map (fun _ -> (default_value tv)))
      )
      in bind x v acc) botenv vdl 

let init_sysstate = { 
    accounts = (fun a -> failwith ("account " ^ a ^ " unbound")); 
    stackenv = [botenv];
    blocknum = 0;
    active = []; 
}

let exec_cmd (n_steps : int) (c : cmd) (a : addr) (st : sysstate) : exec_state =
  let rec exec_rec_cmd n s =
    if n<=0 then s
    else try
        let s' = step_cmd s
        in exec_rec_cmd (n-1) s'
      with NoRuleApplies -> s
    in exec_rec_cmd n_steps (Cmd (c,st,a))

let trace_cmd n_steps (c:cmd) (a:addr) (st : sysstate) : exec_state list =
  let rec trace_rec_cmd n t =
    if n<=0 then [t]
    else try
        let t' = step_cmd t
        in t::(trace_rec_cmd (n-1) t')
      with NoRuleApplies -> [t]
  in trace_rec_cmd n_steps (Cmd(c,st,a))


(******************************************************************************)
(* Funds an account in a system state. Creates account if it does not exist   *)
(******************************************************************************)

let faucet (a : addr) (n : int) (st : sysstate) : sysstate = 
  if exists_account st a then 
    let as' = { (st.accounts a) with balance = n + (st.accounts a).balance } in
    { st with accounts = bind a as' st.accounts }
  else
    let as' = { balance = n; storage = botenv; code = None; } in
    { st with accounts = bind a as' st.accounts; active = a::st.active }


(******************************************************************************)
(* Executes steps of a transaction in a system state, returning a trace       *)
(******************************************************************************)

let exec_tx (n_steps : int) (tx: transaction) (st : sysstate) : sysstate =
  if tx.txvalue < 0 then
    failwith ("exec_tx: trying to send a negative amount of tokens")
  else if not (exists_account st tx.txsender) then 
    failwith ("exec_tx: sender address " ^ tx.txsender ^ " does not exist")
  else if (st.accounts tx.txsender).balance < tx.txvalue then
    failwith ("exec_tx: sender address " ^ tx.txsender ^ " has not sufficient balance")
  else if not (exists_account st tx.txto) && tx.txfun <> "constructor" then
    failwith ("exec_tx: to address " ^ tx.txto ^ " does not exist")
  else if (exists_account st tx.txto) && tx.txfun = "constructor" then
    failwith ("exec_tx: calling constructor in already deployed contract at address " ^ tx.txto) 
  else 
  let (sender_state : account_state) = 
    { (st.accounts tx.txsender) with balance = (st.accounts tx.txsender).balance - tx.txvalue } in
  (* sets state of to address. If not created yet, deploys the contract *)
  let (to_state : account_state),(deploy : bool) = 
    if exists_account st tx.txto 
    then    { (st.accounts tx.txto) with balance = (st.accounts tx.txto).balance + tx.txvalue }, 
            false (* deploy=false ==> cannot call constructor *) 
    else (match tx.txargs with 
      | Addr(code)::_ ->
          (try let c = code |> parse_contract |> blockify_contract in 
            { balance=tx.txvalue; storage = init_storage c; code = Some c }, 
            true (* deploy=true ==> must call constructor *)
          with _ -> failwith ("exec_tx: syntax error in contract code: " ^ code))
      | _ -> failwith "exec_tx: the first parameter of a deploy transaction must be the contract code") in
  match to_state.code with
  | None -> failwith "Called address is not a contract"
  | Some src -> (match find_fun_in_contract src tx.txfun with
    | None when (not deploy) -> failwith ("Contract at address " ^ tx.txto ^ " has no function named " ^ tx.txfun)
    | None -> (* deploy a contract with no constructor (non-payable) *)
      if tx.txvalue > 0 then 
        failwith "The deployed contract should have a payable constructor if you send value"
      else
        { accounts = st.accounts 
            |> bind tx.txsender sender_state
            |> bind tx.txto to_state; 
          stackenv = st.stackenv;
          blocknum = 0;
          active = tx.txto :: st.active }
    | Some (Proc(_,xl,c,_,p,_))
    | Some (Constr(xl,c,p)) ->
        if not p && tx.txvalue>0 then 
          failwith "exec_tx: sending ETH to a non-payable function"
      else
        let xl',vl' =
          if deploy then match tx.txargs with 
            _::al -> 
            (VarT(AddrBT false,false),"msg.sender") :: xl,
            Addr (tx.txsender) :: al
            | _ -> assert(false) (* should not happen *)
          else
            (VarT(AddrBT false,false),"msg.sender") :: (VarT(IntBT,false),"msg.value") :: xl,
            Addr tx.txsender :: Int tx.txvalue :: tx.txargs
        in
        let e' = bind_fargs_aargs xl' vl' in
        let st' = { accounts = st.accounts 
                      |> bind tx.txsender sender_state
                      |> bind tx.txto to_state; 
                    stackenv = e' :: st.stackenv;
                    blocknum = 0;
                    active = if deploy then tx.txto :: st.active else st.active } in
        try (match exec_cmd n_steps c tx.txto st' with
          | St st'' -> st'' |> popenv
          | Reverted -> st  (* if the command reverts, the new state is st *)
          | _ -> st (* exec_tx: execution of command not terminated (not enough gas?) => revert *)
        )
        with _ -> st (* exception thrown during execution of command => revert *)
    ) 

let exec_tx_list (n_steps : int) (txl : transaction list) (st : sysstate) = 
  List.fold_left 
  (fun sti tx -> exec_tx n_steps tx sti)
  st
  txl

(******************************************************************************)
(*                       Deploys a contract in a system state                 *)
(******************************************************************************)

let deploy_contract (tx : transaction) (src : string) (st : sysstate) : sysstate =
  if exists_account st tx.txto then 
    failwith ("deploy_contract: address " ^ tx.txto ^ " already bound in sysstate")
  else if tx.txfun <> "constructor" then
    failwith ("deploy_contract: deploying a contract must call the constructor")
  else let tx' = { tx with txargs = Addr(src) :: tx.txargs }
  in exec_tx 1000 tx' st
