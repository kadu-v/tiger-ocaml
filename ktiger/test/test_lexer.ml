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
       ]

let () = run_test_tt_main test_tokens
