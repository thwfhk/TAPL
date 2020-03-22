(* module Syntax: syntax trees and associated support functions *)

(* Data type definitions *)
type ty = 
  TyBool
| TyArr of ty * ty

type term =
  TmTrue
| TmFalse
| TmIf of term * term * term
| TmVar of int * int            (* De Bruijin index, current contex length *)
| TmAbs of string * ty * term        (* original name, term *)
| TmApp of term * term

type binding =
  NameBind 
| VarBind of ty

type context = (string * binding) list

type command =
| Eval of term
| Bind of string * binding

(* Contexts *)
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

val getTypeFromContext: context -> int -> ty

(* Printing *)
val printType: ty -> unit
val printValue: context -> term -> unit
val print: string -> unit
val pr: string -> unit
val error: string -> 'a

