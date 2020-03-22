(* ---------------------------------------------------------------------- *)
(* Datatypes *)

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

(* ---------------------------------------------------------------------- *)
(* Utilities *)

exception Exit

let pr = output_string stdout

let print x = output_string stdout x; flush stdout

let error x = pr x; pr " "; raise Exit

(* ---------------------------------------------------------------------- *)
(* Context management *)

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx x bind = (x,bind)::ctx (* 每次加入开头，index是0 *)

let addname ctx x = addbinding ctx x NameBind

let rec printctx ctx = 
  match ctx with
      [] -> ()
    | (x,_)::rest ->
        print x; print " "; printctx rest

let rec isnamebound ctx x =
  match ctx with
      [] -> false
    | (y,_)::rest ->
        if y=x then true
        else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind)::ctx), x

let index2name ctx x =
  try
    let (xn,_) = List.nth ctx x in xn
  with Failure _ -> error "Variable lookup failure! "

let rec name2index ctx x =
  match ctx with
      [] -> error ("Identifier " ^ x ^ " is unbound! ")
    | (y,_)::rest ->
        if y=x then 0
        else 1 + (name2index rest x)

(* ---------------------------------------------------------------------- *)
(* Shifting *)

(* 对于var应用onvar(substitution或者shift)，abs和app继续，c记录当前是第几层abs(0开始) *)
let tmmap onvar c t = 
  let rec walk c t = match t with
      TmVar(x, n) -> onvar c x n
    | TmAbs(s, ty, t) -> TmAbs(s, ty, walk (c+1) t)
    | TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)
    | TmTrue -> TmTrue
    | TmFalse -> TmFalse
    | TmIf(t1, t2, t3) -> TmIf(walk c t1, walk c t2, walk c t3)
  in walk c t

(* \uparrow^d_c(t) *)
let termShiftAbove d c t =
  tmmap 
    (fun c x n -> if x >= c then TmVar(x+d, n+d) else TmVar(x, n+d))
    c t

let termShift d t = termShiftAbove d 0 t

(* ---------------------------------------------------------------------- *)
(* Substitution *)

(* [j->s]t *)
let termSubst j s t =
  tmmap
    (fun c x n -> if x=j+c then termShift c s else TmVar(x,n))
    0 t

(* [0->s]t加上外面去掉一层lambda *)
let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)

(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)

let rec getbinding ctx i =
  try
    let (_,bind) = List.nth ctx i in
    bind 
  with Failure _ -> error "Variable lookup failure! "

let rec getTypeFromContext ctx i =
  match getbinding ctx i with
    VarBind(ty) -> ty
  | _ -> error "getTypeFromContext Error: Wrong kind of binding for variable "

(* ---------------------------------------------------------------------- *)

let rec printType ty = match ty with
    TyBool -> 
      pr "Bool"
  | TyArr(ty1, ty2) ->
      printType ty1; pr "->"; printType ty2

let rec printValue ctx t = match t with
    TmVar(x, n) ->
      if ctxlength ctx = n then
        pr (index2name ctx x)
      else
        error ("Unconsistency found when printing! ")
  | TmApp(t1, t2) ->
      printValue ctx t1;
      pr " ";
      printValue ctx t2
  | TmAbs(x, tyT1, t2) -> 
      let (ctx', x') = pickfreshname ctx x in
      pr "(lambda "; pr x'; pr ":"; printType tyT1;
      pr "."; printValue ctx' t2; pr ")"
  | TmTrue -> 
      pr "true"
  | TmFalse ->
      pr "false"
  | TmIf(t1, t2, t3) ->
      pr "if "; printValue ctx t1; pr " then "; printValue ctx t2; pr " else "; printValue ctx t3
  (*| _ -> pr "Non-value found when printing! "; raise Exit*)

