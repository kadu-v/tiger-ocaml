open Types
open Absyn
open Symbol
open Env

let check_int ty loc = if not (equiv_ty ty INT) then ()

let rec check_exp exp venv tenv =
  match exp with
  | NilExp { loc } -> NIL
  | IntExp { loc } -> INT
  | StringExp { loc } -> STRING
  | BreakExp { loc } -> UNIT
  | OpExp { oper; left; right; loc } -> (
      let ty1 = check_exp left venv tenv in
      let ty2 = check_exp right venv tenv in
      match oper with
      | PlusOp | MinusOp | TimesOp | DivideOp ->
          check_int ty1 INT;
          check_int ty2 INT;
          INT
      | OrOp | AndOp ->
          check_int ty1 INT;
          check_int ty2 INT;
          INT
      (* TODO: 文字列や参照の比較もできるように変更 *)
      | LtOp | LteOp | GtOp | GteOp | EqOp | NeqOp ->
          check_int ty1 INT;
          check_int ty2 INT;
          INT )
  | SeqExp { exps } ->
      let ty =
        List.fold_left (fun _ exp1 -> check_exp exp1 venv tenv) UNIT exps
      in
      ty
  | VarExp { var } -> check_var var venv
  | _ -> Error.error (show_exp exp)

and check_var var venv =
  match var with
  | SimpleVar { var; loc } -> (
      let sym = symbol var in
      match look sym venv with Some ty -> ty | None -> Error.error var )
  (* | FieldVar { var; mem; loc } -> (
      let ty1 = check_var var venv in
      match ty1 with *)
  (* エラーメッセージを親切に*)
  | _ -> Error.error (show_var var)
