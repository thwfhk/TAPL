type term = 
  TmTrue
| TmFalse
| TmIf of term * term * term
| TmZero
| TmSucc of term
| TmPred of term
| TmIsZero of term

exception StuckTerm
exception NotValue

val print: string -> unit
val printValue: term -> unit