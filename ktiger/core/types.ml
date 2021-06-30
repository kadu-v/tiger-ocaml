type unique = unit ref [@@deriving show]

type ty =
  | INT
  | STRING
  | UNIT
  | NIL
  | RECORD of (Symbol.symbol * ty) list * unique
[@@deriving show]

(*   | ARRAY of ty * unique
   | NAME of Symbol.symbol * ty option ref 
*)

let actual_ty t = t

let equiv_ty ty1 ty2 =
  match (ty1, ty2) with
  | INT, INT -> true
  | STRING, STRING -> true
  | UNIT, UNIT -> true
  | NIL, NIL -> true
  | NIL, RECORD _ -> true
  | RECORD (_, u1), RECORD (_, u2) -> u1 = u2
  | _ -> false
