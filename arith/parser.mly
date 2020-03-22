%{
open Syntax
%}

/* declarations */
%token TRUE FALSE IF THEN ELSE 
%token SUCC PRED ISZERO
%token SEMI LPAREN RPAREN EOF
%token ZERO

%nonassoc IF THEN ELSE
%right SUCC PRED ISZERO

%start main cmd
%type <Syntax.term list> main cmd

%% 
/* rules */
cmd :
    EOF {[]} /* just the end of the input, do nothing */
  | Term SEMI cmd {$1 :: $3}
  | Term SEMI SEMI {$1 :: []} /* return when encountered ;; */

main :
    EOF { [] }
  | Term SEMI main { $1 :: $3 }

Term :
    TRUE { TmTrue }
  | FALSE { TmFalse }
  | ZERO { TmZero }
  | LPAREN Term RPAREN { $2 }
  | SUCC Term { TmSucc $2 }
  | PRED Term { TmPred $2 }
  | ISZERO Term { TmIsZero $2 }
  | IF Term THEN Term ELSE Term { TmIf($2, $4, $6) }