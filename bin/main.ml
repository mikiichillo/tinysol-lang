open TinysolLib.Main
open TinysolLib.Sysstate
open TinysolLib.Utils
open TinysolLib.Cli
open TinysolLib.Prettyprint
open TinysolLib.Typechecker
;;

match Array.length(Sys.argv) with
(* parse_cmd *) 
  2 when Sys.argv.(1)="parse_cmd" -> (match read_line() with
      Some s when s<>"" -> s |> parse_cmd |> string_of_cmd |> print_string
    | _ -> print_newline())
(* exec_cmd *)
| 3 when Sys.argv.(1)="exec_cmd" -> (match read_line() with
    | Some s when s<>"" -> s |> parse_cmd |> blockify_cmd (* TODO: enumify? *)
      |> fun c -> trace_cmd (int_of_string Sys.argv.(2)) c 
      (push_callstack {callee="0xC"; locals=[];} init_sysstate)
      |> string_of_trace |> print_string
    | _ -> print_newline())
(* parse_contract *) 
| 2 when Sys.argv.(1)="parse" -> (match read_line() with
    | Some s when s<>"" -> s |> parse_contract |> preprocess_contract |> string_of_contract |> print_string
    | _ -> print_newline())
| 3 when Sys.argv.(1)="parse" -> (match read_file Sys.argv.(2) with
      "" -> print_newline()
    | s -> s |> parse_contract |> preprocess_contract |> string_of_contract |> print_string)
(* typecheck contract*)
| 2 when Sys.argv.(1)="typecheck" -> (match read_line() with
    | Some s when s<>"" -> s |> parse_contract |> preprocess_contract |> typecheck_contract 
             |> string_of_typecheck_result |> print_endline 
    | _ -> print_newline())
| 3 when Sys.argv.(1)="typecheck" -> (
    contract_names_in_file Sys.argv.(2)
    |>
    List.map (fun cn -> read_contract_in_file cn Sys.argv.(2))
    |> 
    List.iter (fun s -> 
      s |> parse_contract |> preprocess_contract |> typecheck_contract 
        |> string_of_typecheck_result |> print_endline)
    ) 
(* unittest *)
(* HARDCODED TEST per la Issue 4 *)
| 2 when Sys.argv.(1)="test_issue4" ->
    print_endline "--- AVVIO TEST MANUALE (Bypass Parser) ---";
    
    (* Definiamo i comandi manualmente specificando il percorso completo *)
    let cmds = [
      (* 1. Diamo i soldi a 0x1 *)
      TinysolLib.Cli_ast.Faucet("0x1", 1000);

      (* 2. Deploy del contratto TestConst *)
      TinysolLib.Cli_ast.Deploy(
        { 
          TinysolLib.Ast.txsender="0x1"; 
          TinysolLib.Ast.txto="0x2"; 
          TinysolLib.Ast.txvalue=0; 
          TinysolLib.Ast.txfun="constructor"; 
          TinysolLib.Ast.txargs=[] 
        }, 
        "TestConst", 
        "contracts/test_const.sol"
      );

      (* 3. Revert: Questa azione DEVE fallire *)
      TinysolLib.Cli_ast.Revert(
        { 
          TinysolLib.Ast.txsender="0x1"; 
          TinysolLib.Ast.txto="0x2"; 
          TinysolLib.Ast.txvalue=0; 
          TinysolLib.Ast.txfun="breakRules"; 
          TinysolLib.Ast.txargs=[] 
        }
      )
    ] in

    (* 2. Eseguiamo la lista di comandi *)
    let (_, final_state) = exec_cli_cmd_list true cmds init_sysstate in
    
    (* 3. Stampiamo il risultato *)
    print_endline "\n--- TEST COMPLETATO CON SUCCESSO! ---";
    final_state |> string_of_sysstate [] |> print_string
(* wrong usage *)      
| _ -> print_string "Usage:
  dune exec tinysol parse_cmd         : parses cmd in stdin
  dune exec tinysol exec_cmd <n>      : executes n_steps of cmd in stdin
  dune exec tinysol parse [<file>]    : parses contract in file or stdin
  dune exec tinysol typecheck [<file>]: typechecks contract in file or stdin
  dune exec tinysol unittest <file>   : executes CLI commands from file 
"
