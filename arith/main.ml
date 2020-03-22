open Syntax
open Core

exception ParseError

let parse_channel inChannel = 
  let lexbuf = Lexing.from_channel inChannel in
  let result =
    try Parser.main Lexer.token lexbuf 
    with Parsing.Parse_error -> raise ParseError
  in
  Parsing.clear_parser(); close_in inChannel; result

let rec process_term t = printValue (eval t)
  
let process_channel f  =
  let cmds = parse_channel f in
  let g t = process_term t; print "\n" in
  List.iter g cmds

let process_REPL () = 
  let lexbuf = Lexing.from_channel stdin in
  while true do
    print "> "; flush stdout;
    let result = 
      try Parser.cmd Lexer.token lexbuf
      with Parsing.Parse_error -> raise ParseError
    in
    if result = [] then exit 0
    else let g t = process_term t; print "\n"; flush stdout in List.iter g result
  done


exception NoInputFile

let main () = 
  if Array.length Sys.argv < 2 then
    let () = print "You must specify an input file or use the '-i' mode!\n" in
    raise NoInputFile
  else if Sys.argv.(1) = "-i" then process_REPL ()
  else process_channel (open_in Sys.argv.(1))

let () = main ()