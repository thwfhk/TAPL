(* header *)
{
let keyword_table = Hashtbl.create 32
let () = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
  [ ("if", Parser.IF);
    ("then", Parser.THEN);
    ("else", Parser.ELSE);
    ("true", Parser.TRUE);
    ("false", Parser.FALSE);
    ("succ", Parser.SUCC);
    ("pred", Parser.PRED);
    ("iszero", Parser.ISZERO);

    (";", Parser.SEMI);
    ("(", Parser.LPAREN); 
    (")", Parser.RPAREN); ]

let find_in_table id = 
  try Hashtbl.find keyword_table id
  with Not_found -> output_string stdout "Undefined keywords."; raise Not_found
}

(* rules *)
rule token = parse 
    [' ' '\t' '\n']+            { token lexbuf }
  | [' ' '\t' '\n']*('\r')?'\n' { token lexbuf }
  | '0'                         { Parser.ZERO }
  | ['a'-'z']+ as id            { find_in_table id }
  | [';' '(' ')'] as id         { find_in_table (String.make 1 id) }
  | eof                         { Parser.EOF }