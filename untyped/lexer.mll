(* 
   The lexical analyzer: lexer.ml is generated automatically
   from lexer.mll.
   
   The only modification commonly needed here is adding new keywords to the 
   list of reserved words at the top.  
*)

{

let reservedWords = [
  (* Keywords *)
  ("lambda", Parser.LAMBDA);
  ("true", Parser.TRUE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("else", Parser.ELSE);
  ("iszero",Parser.ISZERO);
  
  (* Symbols *)
  (".", Parser.DOT);
  (";", Parser.SEMI);
  ("/", Parser.SLASH);
  ("(", Parser.LPAREN); 
  (")", Parser.RPAREN);
  ("+", Parser.ADD);
  ("-", Parser.SUB);
  ("*", Parser.MUL)
]

(* Support functions *)

let (symbolTable : (string, Parser.token) Hashtbl.t) = Hashtbl.create 1024
let () = List.iter (fun (str,tok) -> Hashtbl.add symbolTable str tok) reservedWords

let createID str =
  try (Hashtbl.find symbolTable str)
  with _ ->
    if (String.get str 0) >= 'A' && (String.get str 0) <= 'Z' then
       Parser.UCID str
    else 
       Parser.LCID str

let text = Lexing.lexeme

}

(* The main body of the lexical analyzer *)

rule token = parse
  [' ' '\t' '\n']+     { token lexbuf }

| ['0'-'9']+
    { Parser.INTV (int_of_string (text lexbuf)) }

| ['A'-'Z' 'a'-'z' '_']
  ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*
    { createID (text lexbuf) }

| ['/' '(' ')' '.' ';' '_' '+' '-' '*']
    { createID (text lexbuf) }

| eof { Parser.EOF }

(*| _  { print "Illegal character"; }*)

(*  *)
