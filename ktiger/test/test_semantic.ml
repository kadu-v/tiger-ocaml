open OUnit2
open Util
open Core.Types
open Core.Env
open Core.Symbol

let test_semantic_literal =
  "test_semantic_literal"
  >::: [
         test_check_exp "nil" "nil" base_venv base_tenv NIL;
         test_check_exp "break" "break" base_venv base_tenv UNIT;
         test_check_exp "1" "1" base_venv base_tenv INT;
         test_check_exp "120" "120" base_venv base_tenv INT;
         test_check_exp "bar" "\"bar\"" base_venv base_tenv STRING;
         test_check_exp "foo" "\"foo\"" base_venv base_tenv STRING;
       ]

let test_semantic_opexp =
  "test_semantic_opexp"
  >::: [
         test_check_exp "1+1" "1+1" base_venv base_tenv INT;
         test_check_exp "1-2" "1-2" base_venv base_tenv INT;
         test_check_exp "11*13" "11*13" base_venv base_tenv INT;
         test_check_exp "13/7" "13/7" base_venv base_tenv INT;
         test_check_exp "1&0" "1&0" base_venv base_tenv INT;
         test_check_exp "1|2" "1|2" base_venv base_tenv INT;
         test_check_exp "1=1" "1=1" base_venv base_tenv INT;
         test_check_exp "1<>2" "1<>2" base_venv base_tenv INT;
         test_check_exp "11<=13" "11<=13" base_venv base_tenv INT;
         test_check_exp "13<7" "13<7" base_venv base_tenv INT;
         test_check_exp "11>=13" "11>=13" base_venv base_tenv INT;
         test_check_exp "13>7" "13>7" base_venv base_tenv INT;
       ]

let test_semantic_ifexp =
  "test_semantic_ifexp"
  >::: [
         test_check_exp "ifexp1" "if 1 = 1 then 1+1 else 2" base_venv base_tenv
           INT;
         test_check_exp "ifexp2" "if 1 + 1 then 1 * 3 else 100" base_venv
           base_tenv INT;
         test_check_exp "ifexp3" "if 1 > 0 then \"xx\" else \"yyy\"" base_venv
           base_tenv STRING;
         (* test_check_exp "1|2" "1|2" base_venv base_tenv INT;
            test_check_exp "1=1" "1=1" base_venv base_tenv INT;
            test_check_exp "1<>2" "1<>2" base_venv base_tenv INT;
            test_check_exp "11<=13" "11<=13" base_venv base_tenv INT;
            test_check_exp "13<7" "13<7" base_venv base_tenv INT;
            test_check_exp "11>=13" "11>=13" base_venv base_tenv INT;
            test_check_exp "13>7" "13>7" base_venv base_tenv INT; *)
       ]

let test_semantic_seqexp =
  "test_semantic_seqexp"
  >::: [
         test_check_exp "(1;1)" "(1;1)" base_venv base_tenv INT;
         test_check_exp "(1+1;1/10)" "(1-1;1*2)" base_venv base_tenv INT;
         test_check_exp "(1>1;\"xx\")" "(1>1;\"xx\")" base_venv base_tenv STRING;
         test_check_exp "(1+1;nil)" "(1-1;nil)" base_venv base_tenv NIL;
       ]

(* let record1 = let sym = symbol "rcd1" in RECORD () *)
(* let var_venv =
  List.fold_left
    (fun acc (x, ty) -> enter (symbol x) ty acc)
    base_venv
    [ ("x", INT); ("foo", STRING) ]

let test_semantic_var =
  "test_semantic_var"
  >::: [
         test_check_exp "x" "x" var_venv base_tenv INT;
         test_check_exp "foo" "foo" var_venv base_tenv STRING;
         (* test_checkexp "rc1" "rc1" var_venv base_tenv RECORD *)
       ] *)

let test_sematics =
  "test_semantics"
  >::: [
         test_semantic_literal;
         test_semantic_opexp;
         test_semantic_seqexp;
         test_semantic_ifexp;
       ]

let () = run_test_tt_main test_sematics
