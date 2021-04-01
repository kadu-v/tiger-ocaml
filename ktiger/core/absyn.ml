open Location

(* 
type ty =
  | NameTy of string * position
  | RecordTy of field list
  | ArrayTy of string * position
[@@derinving show, eq] *)

type exp =
  | NilExp of { loc : location }
  | IntExp of { value : int; loc : location }
  | StringExp of { str : string; loc : location }
  | OpExp of { oper : oper; left : exp; right : exp; loc : location }
  | SeqExp of { exps : exp list }
  | CallExp of { func : string; args : exp list; loc : location }
  | VarExp of { var : var }
  | RecordExp of {
      typ : string;
      fields : (string * exp * location) list;
      loc : location;
    }
  | AssignExp of { var : var; exp : exp; loc : location }
  | IfExp of { test : exp; then' : exp; else' : exp option; loc : location }
  | WhileExp of { test : exp; body : exp; loc : location }
  | ForExp of {
      var : string;
      escape : bool ref;
      lo : exp;
      hi : exp;
      body : exp;
      loc : location;
    }
  | BreakExp of { loc : location }
  | ArrayExp of { typ : string; size : exp; init : exp; loc : location }
  | LetExp of { decs : dec list; body : exp; loc : location }
[@@deriving show, eq]

and dec =
  | FunctionDec of fundec list
  | VarDec of {
      name : string;
      escape : bool ref;
      typ : (string * location) option;
      init : exp;
      loc : location;
    }
  | TypeDec of tydec list

and fundec = {
  fname : string;
  fparams : field list;
  fresult : (string * location) option;
  fbody : exp;
  floc : location;
}
[@@deriving show, eq]

and field = { name : string; escape : bool ref; typ : string; loc : location }
[@@deriving show, eq]

and tydec = { tname : string; tty : ty; tloc : location } [@@deriving show, eq]

and ty =
  | NameTy of { name : string; loc : location }
  | RecordTy of { fields : field list }
  | ArrayTy of { name : string; loc : location }
[@@deriving show, eq]

and var =
  | SimpleVar of { var : string; loc : location }
  | FieldVar of { var : var; mem : string; loc : location }
  | SubscriptVar of { var : var; exp : exp; loc : location }
[@@deriving show, eq]

and oper =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | OrOp
  | AndOp
  | LtOp
  | LteOp
  | GtOp
  | GteOp
  | EqOp
  | NeqOp
[@@deriving show, eq]
