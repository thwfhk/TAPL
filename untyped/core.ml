open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

let rec isval ctx t = match t with
    TmTrue -> true
  | TmFalse -> true
  | TmInt(_) -> true
  | TmAbs(_,_) -> true
  | _ -> false

exception NoRuleApplies

let rec eval1 ctx t = match t with
    TmIf(TmTrue, t2, t3) ->
      t2
  | TmIf(TmFalse, t2, t3) ->
      t3
  | TmIf(t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(t1', t2, t3)
  | TmApp(TmAbs(x, t12), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(v1, t2')
  | TmApp(t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(t1', t2)

  | TmAdd(v1, v2) when (isval ctx v1 && isval ctx v2) ->
      let TmInt(x1) = v1 in
      let TmInt(x2) = v2 in
      TmInt(x1 + x2)
  | TmAdd(v1, t2) when isval ctx v1 ->
      let t2' =eval1 ctx t2 in
      TmAdd(v1, t2')
  | TmAdd(t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmAdd(t1', t2)

  | TmSub(v1, v2) when (isval ctx v1 && isval ctx v2) ->
      let TmInt(x1) = v1 in
      let TmInt(x2) = v2 in
      TmInt(x1 - x2)
  | TmSub(v1, t2) when isval ctx v1 ->
      let t2' =eval1 ctx t2 in
      TmSub(v1, t2')
  | TmSub(t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmSub(t1', t2)

  | TmMul(v1, v2) when (isval ctx v1 && isval ctx v2) ->
      let TmInt(x1) = v1 in
      let TmInt(x2) = v2 in
      TmInt(x1 * x2)
  | TmMul(v1, t2) when isval ctx v1 ->
      let t2' =eval1 ctx t2 in
      TmMul(v1, t2')
  | TmMul(t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmMul(t1', t2)

  | TmIsZero(v) when isval ctx v ->
      (match v with
        | TmInt(0) -> TmTrue
        | _ -> TmFalse)
  | TmIsZero(t) ->
      let t' = eval1 ctx t in
      TmIsZero(t')
  | _ ->
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t
