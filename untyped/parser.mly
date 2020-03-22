/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* Keyword tokens */
%token LAMBDA
%token TRUE FALSE IF THEN ELSE
%token ISZERO
%token ADD SUB MUL

%left ADD SUB
%left MUL

/* Identifier and constant value tokens */
%token <string> UCID  /* uppercase-initial */
%token <string> LCID  /* lowercase/symbolic-initial */
%token <int> INTV

/* Symbolic tokens */
%token DOT
%token EOF
%token LPAREN
%token RPAREN
%token USCORE
%token SEMI
%token SLASH

/* ---------------------------------------------------------------------- */

%start toplevel, cmd
%type < Syntax.context -> (Syntax.command list * Syntax.context) > toplevel, cmd

%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

cmd :
    EOF { fun ctx -> [],ctx } /* just the end of the input, do nothing */
  | Command SEMI cmd 
        { fun ctx ->
          let cmd,ctx = $1 ctx in
          let cmds,ctx = $3 ctx in
          cmd::cmds,ctx }
  | Command SEMI SEMI 
        { fun ctx ->
          let cmd,ctx = $1 ctx in
          cmd::[],ctx } /* return when encountered ;; */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { fun ctx -> [],ctx }
  | Command SEMI toplevel
      { fun ctx ->
          let cmd,ctx = $1 ctx in
          let cmds,ctx = $3 ctx in
          cmd::cmds,ctx }

/* A top-level command */
Command :
  | Term 
      { fun ctx -> (let t = $1 ctx in Eval(t)),ctx }
  | LCID Binder
      { fun ctx -> (Bind($1, $2 ctx)), addname ctx $1 }
      /* 后面的addname是为了term生成的时候 
         main.ml里处理的时候把term生成后的ctx丢掉了，处理到这个语句时要重新执行一次这个command  */

/* Right-hand sides of top-level bindings */ 
Binder :
    SLASH
      { fun ctx -> NameBind }

Term :
    AppTerm
      { $1 }
  | LAMBDA LCID DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx $2 in
          TmAbs($2, $4 ctx1) }
  | LAMBDA USCORE DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs("_", $4 ctx1) }
  | IF Term THEN Term ELSE Term
      { fun ctx -> TmIf($2 ctx, $4 ctx, $6 ctx) }

  | Term ADD Term
      { fun ctx -> TmAdd($1 ctx, $3 ctx) }
  | Term SUB Term
      { fun ctx -> TmSub($1 ctx, $3 ctx) }
  | Term MUL Term
      { fun ctx -> TmMul($1 ctx, $3 ctx) }
      
AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { fun ctx -> TmApp($1 ctx, $2 ctx) }
  | ISZERO ATerm
      { fun ctx -> TmIsZero($2 ctx) }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN Term RPAREN  
      { $2 } 
  | LCID 
      { fun ctx -> TmVar(name2index ctx $1, ctxlength ctx) }    
  | TRUE
      { fun ctx -> TmTrue }
  | FALSE 
      { fun ctx -> TmFalse }
  | INTV
      { fun ctx -> TmInt($1) }


/*   */
