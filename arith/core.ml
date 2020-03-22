open Syntax

exception NoRuleApplies

let rec isnumericval t = match t with
    TmZero -> true
  | TmSucc(t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue  -> true
  | TmFalse -> true
  | t when isnumericval t  -> true
  | _ -> false

let rec eval1 t = match t with (* single step evaluation *)
    TmIf(TmTrue,t2,t3) -> t2
  | TmIf(TmFalse,t2,t3) -> t3
  | TmIf(t1,t2,t3) ->
      let t1' = eval1 t1 in
      TmIf(t1', t2, t3)
  | TmSucc(t1) ->
      let t1' = eval1 t1 in
      TmSucc(t1')
  | TmPred(TmZero) -> TmZero
  | TmPred(TmSucc(nv)) when (isnumericval nv) -> nv
  | TmPred(t1) ->
      let t1' = eval1 t1 in
      TmPred(t1')
  | TmIsZero(TmZero) -> TmTrue
  | TmIsZero(TmSucc(nv)) when (isnumericval nv) -> TmFalse
  | TmIsZero(t1) ->
      let t1' = eval1 t1 in
      TmIsZero(t1')
  | _ -> raise NoRuleApplies

let rec eval t =
  try let t' = eval1 t in eval t'
  with NoRuleApplies -> t

(* 这里有个问题：比如iszero true这样的stuck，由于t1->t1'这样一步并没有，会raise NRA，然后就会原样返回这个term了 *)
(* 所以eval里遇到stuck是原样返回的 *)
(* 原来的arith里甚至也原样打印了2333 *)