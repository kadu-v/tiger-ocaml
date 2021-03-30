open Core.Lexer
open Core.Parser
open Core.Absyn

(* open Core.Absyn *)
open Core.Util
open OUnit2

let test_token name expected input =
  let lexbuf = Lexing.from_string input in
  let tok = token lexbuf in
  name >:: fun _ ->
  assert_equal expected tok ~printer:show_token ~cmp:equal_token

let rec equal_token_list toks1 toks2 =
  match (toks1, toks2) with
  | [], [] | [ EOF ], [] | [], [ EOF ] -> true
  | tok1 :: xs, tok2 :: ys ->
      if equal_token tok1 tok2 then equal_token_list xs ys else false
  | _ -> false

let test_token_list name (expected : token list) (input : string) =
  name >:: fun _ ->
  assert_equal expected (string_tokenizer input) ~printer:show_token_list
    ~cmp:equal_token_list

let test_parser name input expected =
  let ast = string_parser input in
  name >:: fun _ -> assert_equal expected ast ~printer:show_exp ~cmp:equal_exp
