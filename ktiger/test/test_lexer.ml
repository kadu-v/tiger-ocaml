open OUnit2
open Util

let token_tests =
  "test of lexer"
  >::: [
         token_list_test "Keywords"
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
         token_list_test "Separation symbol" [] "";
       ]

let () = run_test_tt_main token_tests
