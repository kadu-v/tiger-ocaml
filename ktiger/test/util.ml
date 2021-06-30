open Core.Lexer
open Core.Parser
open Core.Absyn
open Core.Semantic
open Core.Types

(* open Core.Absyn *)
open Core.Util
open OUnit2

(* Compare the lexical analysis of input string with the token *)
let test_token name expected input =
  let lexbuf = Lexing.from_string input in
  let tok = token lexbuf in
  name >:: fun _ ->
  assert_equal expected tok ~printer:show_token ~cmp:equal_token

(* Compare the elements of two lists of lexemes  *)
let rec equal_token_list toks1 toks2 =
  match (toks1, toks2) with
  | [], [] | [ EOF ], [] | [], [ EOF ] -> true
  | tok1 :: xs, tok2 :: ys ->
      if equal_token tok1 tok2 then equal_token_list xs ys else false
  | _ -> false

(* Compare the lexcical analysis of input string with the expected token list  *)
let test_token_list name (expected : token list) (input : string) =
  name >:: fun _ ->
  assert_equal expected (string_tokenizer input) ~printer:show_token_list
    ~cmp:equal_token_list

(* Compare the Ast of input string with the expected Ast  *)
let test_parser name input expected =
  let ast = string_parser input in
  name >:: fun _ -> assert_equal expected ast ~printer:show_exp ~cmp:equal_exp

(* Compare the  *)
let test_check_exp name input venv tenv expected =
  let ast = string_parser input in
  let ty = check_exp ast venv tenv in
  name >:: fun _ -> assert_equal expected ty ~printer:show_ty ~cmp:equiv_ty
