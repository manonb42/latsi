open Lib;;


let run_from_channel channel =
    let lexbuf = Lexing.from_channel channel in
    let ast = Parser.input Lexer.main lexbuf in
    Interpret.run ast
;;


let _ =
  let input = if Array.length Sys.argv > 1 then
      open_in Sys.argv.(1)
    else stdin
  in run_from_channel input
;;
