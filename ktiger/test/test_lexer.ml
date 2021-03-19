open OUnit2
open Util

let test_tokens =
  "test of lexer"
  >::: [
         test_token_list "Keywords"
           [
             WHILE;
             FOR;
             TO;
             BREAK;
             LET;
             IN;
             END;
             FUNCTION;
             VAR;
             TYPE;
             ARRAY;
             IF;
             THEN;
             ELSE;
             DO;
             OF;
             NIL;
             EOF;
           ]
           "while for to break let in end function var type array if then else \
            do of nil";
         test_token_list "Separation symbol" [] "";
       ]

let () = run_test_tt_main test_tokens
