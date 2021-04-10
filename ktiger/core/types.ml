type unique = unit ref

type ty = INT | STRING | UNIT | NIL

(* 
   | RECORD of (Symbol.symbol * ty) list * unique
   | ARRAY of ty * unique
   | NAME of Symbol.symbol * ty option ref 
*)
