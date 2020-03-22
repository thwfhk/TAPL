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

let print = output_string stdout

let printValue t = match t with
    TmTrue -> print "true"
  | TmFalse -> print "false"
  | TmZero -> print "0"
  | TmSucc(_) ->
      let rec f n t = match t with
          TmZero -> print (string_of_int n)
        | TmSucc(s) -> f (n+1) s
        | _ -> print "stuck encountered when printing\n"; raise StuckTerm
      in f 0 t
  | _ -> print "non-value encountered when printing\n"; raise NotValue
