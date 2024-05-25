open Lib
open Lexing
open Printf
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

(*  ================================== WARNING ===============================  *)
(* This file has been (lightly) adapter from Menhir's examples and is only *)
(* used to display better error messages. Some simplifications have been made *)
(* and many comments have been removed for brevity. *)
(*  https://gitlab.inria.fr/fpottier/menhir/blob/master/demos/calc-syntax-errors/calc.ml  *)

module I = Parser.MenhirInterpreter

let env checkpoint =
  match checkpoint with
  | I.HandlingError env -> env
  | _ -> assert false

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None -> 0

let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 40 (* max width 43 *)

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None -> "???"


exception Syntax_error

let fail text buffer (checkpoint : _ I.checkpoint) =
  let location = L.range (E.last buffer) in
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  let message = ParserMessages.message (state checkpoint) in
  let message = E.expand (get text checkpoint) message in
  eprintf "%s%s%s%!" location indication message;
  raise Syntax_error


let parse parser text lexbuf =
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = parser lexbuf.lex_curr_p in
  I.loop_handle (fun x -> x) (fail text buffer) supplier checkpoint



let repl () =
  let rec aux p ctx =
    try
      let text =(print_string "> "; flush stdout; read_line ()) in
      if text = "" then aux p ctx else
      let lexbuf = L.init "<stdin>" (Lexing.from_string text) in
      let ligne = parse Parser.Incremental.main_ligne text lexbuf in
      let p = ligne :: p in
      aux p @@ Interpret.step (Programme p) ctx
    with Syntax_error -> aux p ctx
       | Interpret.Exit EndOfInput -> aux p ctx
       | Interpret.Exit Finished ->
          print_string "end has been reached, ctrl+d to exit the repl\n";
          aux p ctx
       | Interpret.TypeError -> print_string "type error\n"; aux p ctx
       | End_of_file -> ()
  in aux [] Interpret.default_ctx

let () =
  if Array.length Sys.argv <= 1 then
    repl ()
  else
    try
        let filename = Sys.argv.(1) in
        let text, lexbuf = L.read filename in
        let programme = parse Parser.Incremental.main text lexbuf in
        Interpret.run programme
    with Syntax_error -> exit 1
