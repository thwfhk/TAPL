(* module Syntax: syntax trees and associated support functions *)

(* Data type definitions *)
type term =
  TmTrue
| TmFalse
| TmIf of term * term * term
| TmVar of int * int
| TmAbs of string * term
| TmApp of term * term
| TmInt of int
| TmAdd of term * term 
| TmSub of term * term
| TmMul of term * term
| TmIsZero of term

type binding =
    NameBind 

type command =
  | Eval of term
  | Bind of string * binding

(* Contexts *)
type context
val emptycontext : context 
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addname: context -> string -> context
val index2name : context -> int -> string
val getbinding : context -> int -> binding
val name2index : context -> string -> int
val isnamebound : context -> string -> bool
val printctx : context -> unit


(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term

(* Printing *)
val printValue: context -> term -> unit
val print: string -> unit
val pr: string -> unit


