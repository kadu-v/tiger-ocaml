type unique = unit ref

type ty = INT | STRING | UNIT | NIL

(* 
   | RECORD of (Symbol.symbol * ty) list * unique
   | ARRAY of ty * unique
   | NAME of Symbol.symbol * ty option ref 
*)

let actual_ty t = t

let equal_ty ty1 ty2 =
  match (ty1, ty2) with
  | INT, INT -> true
  | STRING, STRING -> true
  | UNIT, UNIT -> true
  | NIL, NIL -> true
  | _ -> false
