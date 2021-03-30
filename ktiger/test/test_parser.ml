open OUnit2
open Util

let test_parser =
  "test of parser"
  >::: [
         test_parser "nil" () "nil";
         test_parser "sequencing" () "(nil; nil)";
         test_parser "0" () "0";
         test_parser "10" () "10";
         test_parser "13" () "13";
         test_parser "43" () "43";
         test_parser "hoge" () "\"hoge\"";
         test_parser "foo_" () "\"foo_\"";
         test_parser "bar1" () "\"bar1\"";
         test_parser "-0" () "-0";
         test_parser "-10" () "-10";
         test_parser "-13" () "-13";
         test_parser "-43" () "-43";
         test_parser "hoge()" () "hoge()";
         test_parser "foo_(a)" () "foo_(a)";
         test_parser "bar1(a, y, b)" () "bar1(a, y, b)";
         test_parser "1+1" () "1+1";
         test_parser "1+2*3" () "1+2*3";
         test_parser "2/3+2" () "2/3+2";
         test_parser "13 | 1" () "13 | 1";
         test_parser "1 & 11" () "1 & 11";
         test_parser "1 = 0" () "1 = 0";
         test_parser "13 <> 51" () "13 <> 51";
         test_parser "1 < 11" () "1 < 11";
         test_parser "1 <= 0" () "1 <= 0";
         test_parser "13 > 51" () "13 > 51";
         test_parser "1 >= 11" () "1 >= 11";
         test_parser "1 + 1 | 23 & 4<= 0" () "1 + 1 | 23 & 4 <= 0";
         test_parser "rcd { x = 1}" () "rcd { x = 1}";
         test_parser "person { name = \"bob\"}" () "person { name = \"bob\"}";
         test_parser "foo { x = 1, y = 3}" () "foo { x = 1, y = 3}";
         test_parser "intArray[100] of 0" () "arr[100] of 0";
         test_parser "hoge[N] of 0.0" () "hoge[N] of 0.0";
         test_parser "monad[yy] of \"str\"" () "monaf[yy] of \"str\"";
         test_parser "a" () "a";
         test_parser "rcd.x" () "rcd.x";
         test_parser "array[100]" () "array[100]";
         test_parser "a := 100" () "a := 100";
         test_parser "array[100] := 1" () "array[100] := 1";
         test_parser "rcd.x := 1+1*2" () "rcd.x := 1+1*2";
         test_parser "if 1 < 2 then 1 else 2" () "if 1 < 2 then 1 else 2";
         test_parser "if 1 + 1 < 2 then 1*3 else 1+2" ()
           "if 1 + 1 < 2 then 1*3 else 1+2";
         test_parser "if 1 < 2 then 1 else 2" () "if 1 < 2 then 1 else 2";
         test_parser "if 1 + 1 < 2 then 1*3" () "if 1 + 1 < 2 then 1*3";
         test_parser "if 1 < 2 then if 3 < 4 then 5 else 6" ()
           "if 1 < 2 then if 3 < 4 then 5 else 6";
         test_parser "if 1 < 2 then if 3 < 4 then 5 else 6 else 7" ()
           "if 1 < 2 then if 3 < 4 then 5 else 6 else 7";
         test_parser "while 10 < 100 do 1+3 < 500" ()
           "while 10 < 100 do 1+3 < 500";
         test_parser "while 1 < 2 do 100 + 500" () "while 1 < 2 do 100 + 500";
         test_parser "while 1 < 100 | x > 100 do 1+3 < 500" ()
           "while 1 < 100 | x > 100 do 1+3 < 500";
         test_parser "for x := 1 to 2 do 1;1" () "for x := 1 to 2 do 1;1";
         test_parser "for x := 1 to 200 do 1;1" () "for x := 1 to 2 do 1;1";
         test_parser "for x := 1+1 to a[100] do b[x] := b[x] + 1" ()
           "for x := 1+1 to a[100] do b[x] := b[x] + 1";
         test_parser "break" () "break";
         test_parser "let type = list in 1+1" () "let type = list in 1+1";
         test_parser "let type = {x : int, y : int} in 1+2" ()
           "let type = {x : int, y : int} in 1+2";
         test_parser "let type = array of string in 100" ()
           "let type = array of string in 100";
         test_parser "let var foo := 113 in 3+2" () "let var foo := 113 in 3+2";
         test_parser "let var foo : int := 113 in 3+2" ()
           "let var foo : int := 113 in 3+2";
         test_parser "let function() = 1 in 3+2" ()
           "let function foo() = 1 in 3+2";
         test_parser "let function() : int = 1 in 3+2" ()
           "let function() : int = 1 in 3+2";
       ]

let () = run_test_tt_main test_parser
