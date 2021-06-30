module T = Types
module A = Absyn
module S = Symbol
module E = Env

let check_ty ty1 ty2 =
  if not (T.equiv_ty ty1 ty2) then
    Error.error (T.show_ty ty1 ^ " != " ^ T.show_ty ty2)

let rec check_exp exp venv tenv =
  match exp with
  | A.NilExp { loc } -> T.NIL
  | A.IntExp { loc } -> T.INT
  | A.StringExp { loc } -> T.STRING
  | A.BreakExp { loc } -> T.UNIT
  | A.OpExp { oper; left; right; loc } -> (
      let ty1 = check_exp left venv tenv in
      let ty2 = check_exp right venv tenv in
      match oper with
      | PlusOp | MinusOp | TimesOp | DivideOp ->
          check_ty ty1 T.INT;
          check_ty ty2 T.INT;
          T.INT
      | OrOp | AndOp ->
          check_ty ty1 T.INT;
          check_ty ty2 T.INT;
          T.INT
      (* TODO: 文字列や参照の比較もできるように変更 *)
      | LtOp | LteOp | GtOp | GteOp | EqOp | NeqOp ->
          check_ty ty1 T.INT;
          check_ty ty2 T.INT;
          T.INT )
  | SeqExp { exps } ->
      let ty =
        List.fold_left (fun _ exp1 -> check_exp exp1 venv tenv) UNIT exps
      in
      ty
  | VarExp { var } -> check_var var venv
  | IfExp { test; then'; else'; loc } -> (
      let ty1 = check_exp test tenv venv in
      let ty2 = check_exp then' tenv venv in
      check_ty ty1 T.INT;
      match else' with
      | Some exp1 ->
          let ty3 = check_exp exp1 tenv venv in
          check_ty ty2 ty3;
          ty2
      | None ->
          check_ty ty2 T.UNIT;
          T.UNIT )
  | _ -> Error.error (A.show_exp exp)

and check_var var venv =
  match var with
  | SimpleVar { var; loc } -> (
      let sym = S.symbol var in
      match S.look sym venv with Some ty -> ty | None -> Error.error var )
  (* | FieldVar { var; mem; loc } -> (
      let ty1 = check_var var venv in
      match ty1 with *)
  (* エラーメッセージを親切に*)
  | _ -> Error.error (A.show_var var)
