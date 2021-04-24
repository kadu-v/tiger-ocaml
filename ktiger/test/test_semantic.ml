open OUnit2
open Util
open Core.Types
open Core.Env

let test_semantic_literal =
  "test_semantic_literal"
  >::: [
         test_check_exp "nil" "nil" base_venv base_venv NIL;
         test_check_exp "break" "break" base_tenv base_venv UNIT;
         test_check_exp "1" "1" base_tenv base_venv INT;
         test_check_exp "120" "120" base_tenv base_venv INT;
         test_check_exp "bar" "\"bar\"" base_tenv base_venv STRING;
         test_check_exp "foo" "\"foo\"" base_tenv base_venv STRING;
       ]

let test_semantic_opexp =
  "test_semantic_opexp"
  >::: [
         test_check_exp "1+1" "1+1" base_venv base_venv INT;
         test_check_exp "1-2" "1-2" base_venv base_venv INT;
         test_check_exp "11*13" "11*13" base_venv base_venv INT;
         test_check_exp "13/7" "13/7" base_venv base_venv INT;
         test_check_exp "1=1" "1=1" base_venv base_venv INT;
         test_check_exp "1<>2" "1<>2" base_venv base_venv INT;
         test_check_exp "11<=13" "11<=13" base_venv base_venv INT;
         test_check_exp "13<7" "13<7" base_venv base_venv INT;
         test_check_exp "11>=13" "11>=13" base_venv base_venv INT;
         test_check_exp "13>7" "13>7" base_venv base_venv INT;
       ]

let test_sematics =
  "test_semantics" >::: [ test_semantic_literal; test_semantic_opexp ]

let () = run_test_tt_main test_sematics
