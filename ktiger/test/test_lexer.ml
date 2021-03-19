open OUnit2
open Util

let test_tokens =
  "test of lexer"
  >::: [
         test_token "while" WHILE "while";
         test_token "for" FOR "for";
         test_token "to" TO "to";
         test_token "break" BREAK "break";
         test_token "let" LET "let";
         test_token "in" IN "in";
         test_token "end" END "end";
         test_token "function" FUNCTION "function";
         test_token "var" VAR "var";
         test_token "type" TYPE "type";
         test_token "array" ARRAY "array";
         test_token "if" IF "if";
         test_token "then" THEN "then";
         test_token "else" ELSE "else";
         test_token "do" DO "do";
         test_token "of" OF "of";
         test_token "of" NIL "nil";
         test_token "," COMMA ",";
         test_token ":" COLON ":";
         test_token ";" SEMICOLON ";";
         test_token "(" LPAREN "(";
         test_token ")" RPAREN ")";
         test_token "[" LBRACKET "[";
         test_token "]" RBRACKET "]";
         test_token "{" LBRACE "{";
         test_token "}" RBRACE "}";
         test_token "." DOT ".";
         test_token "+" PLUS "+";
         test_token "-" MINUS "-";
         test_token "*" TIMES "*";
         test_token "/" DIV "/";
         test_token "<>" NEQ "<>";
         test_token "<" LT "<";
         test_token "<=" LTE "<=";
         test_token ">" GT ">";
         test_token ">=" GTE ">=";
         test_token "&" AND "&";
         test_token "|" OR "|";
         test_token ":=" ASSIGN ":=";
         test_token "INT1" (INT 0) "0";
         test_token "INT2" (INT 10) "10";
         test_token "INT3" (INT 2384859) "2384859";
         test_token "id1" (ID "foo") "foo";
         test_token "id2" (ID "bar") "bar";
         test_token "id3" (ID "hogehoge") "hogehoge";
         test_token "str0" (STR "") "\"\"";
         test_token "str1" (STR "foo") "\"foo\"";
         test_token "str2" (STR "bar") "\"bar\"";
         test_token "str3" (STR "hoge") "\"hoge\"";
         test_token "str4" (STR "xx1") "\"xx1\"";
         test_token "str5" (STR "xx_xs1") "\"xx_xs1\"";
         test_token "comment" EOF "/*xxxxx*/";
         test_token "comment-nested" EOF "/*xxxxx /*yyy*/*/";
         test_token "comment-nested-nested" EOF "/*xxxxx /*yyy*/ /*zzzz*/*/";
       ]

let () = run_test_tt_main test_tokens
