open Format
open Syntax

(* ------------------------   EVALUATION  ------------------------ *)

let rec isval ctx t = match t with
    TmTrue -> true
  | TmFalse -> true
  | TmAbs(_,_,_) -> true
  | _ -> false

exception NoRuleApplies

(* eval的时候顺序是重要的，比如TmApp(v1,t2)一定要在TmApp(t1,t2)前面！ *)
let rec eval1 ctx t = match t with
    TmIf(TmTrue, t2, t3) ->
      t2
  | TmIf(TmFalse, t2, t3) ->
      t3
  | TmIf(t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(t1', t2, t3)
  | TmApp(TmAbs(x, tyT11, t12), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(v1, t2')
  | TmApp(t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(t1', t2)
  | _ ->
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t

let rec typeof ctx t = match t with
    TmVar(x, _) -> getTypeFromContext ctx x
  | TmAbs(x, tyT1, t2) -> 
      let ctx' = addbinding ctx x (VarBind(tyT1)) in 
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, tyT2)
  | TmApp(t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in 
      (match tyT1 with
          TyArr(tyT11, tyT12) ->
            if (=) tyT2 tyT11 then tyT12
            else error "parameter type mismatch"
        | _ -> error "arrow type expected")
  | TmTrue -> 
      TyBool
  | TmFalse -> 
      TyBool
  | TmIf(t1, t2, t3) ->
      if (=) (typeof ctx t1) TyBool then
        let tyT2 = typeof ctx t2 in
        if (=) tyT2 (typeof ctx t3) then tyT2
        else error "arms of conditional have different types"
      else error "guard of conditional not a boolean"