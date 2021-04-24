open Types
open Absyn
open Symbol
open Env

let check_int ty loc = if not (equiv_ty ty INT) then ()

let rec check_exp exp tenv venv =
  match exp with
  | NilExp { loc } -> NIL
  | IntExp { loc } -> INT
  | StringExp { loc } -> STRING
  | BreakExp { loc } -> UNIT
  | OpExp { oper; left; right; loc } -> (
      let ty1 = check_exp left tenv venv in
      let ty2 = check_exp right tenv venv in
      match oper with
      | PlusOp | MinusOp | TimesOp | DivideOp ->
          check_int ty1 INT;
          check_int ty2 INT;
          INT
      (* TODO: 文字列や参照の比較もできるように変更 *)
      | OrOp | AndOp | LtOp | LteOp | GtOp | GteOp | EqOp | NeqOp ->
          check_int ty1 INT;
          check_int ty2 INT;
          INT )
  | _ -> Error.error (show_exp exp)
