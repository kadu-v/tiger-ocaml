open OUnit2
open Util
open Core.Absyn
open Core.Location

let dummy_loc = { loc_fname = "dummy"; loc_lnum = 0; loc_bol = 1; loc_cnum = 2 }

let test_parser =
  "test of parser"
  >::: [
         test_parser "nil" "nil" (NilExp { loc = dummy_loc });
         test_parser "0" "0" (IntExp { value = 0; loc = dummy_loc });
         test_parser "10" "10" (IntExp { value = 10; loc = dummy_loc });
         test_parser "13" "13" (IntExp { value = 13; loc = dummy_loc });
         test_parser "43" "43" (IntExp { value = 43; loc = dummy_loc });
         test_parser "hoge" "\"hoge\""
           (StringExp { str = "hoge"; loc = dummy_loc });
         test_parser "foo_" "\"foo_\""
           (StringExp { str = "foo_"; loc = dummy_loc });
         test_parser "bar1" "\"bar1\""
           (StringExp { str = "bar1"; loc = dummy_loc });
         test_parser "-0" "-0"
           (OpExp
              {
                oper = MinusOp;
                left = IntExp { value = 0; loc = dummy_loc };
                right = IntExp { value = 0; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "-10" "-10"
           (OpExp
              {
                oper = MinusOp;
                left = IntExp { value = 0; loc = dummy_loc };
                right = IntExp { value = 10; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "-13" "-13"
           (OpExp
              {
                oper = MinusOp;
                left = IntExp { value = 0; loc = dummy_loc };
                right = IntExp { value = 13; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "-43" "-43"
           (OpExp
              {
                oper = MinusOp;
                left = IntExp { value = 0; loc = dummy_loc };
                right = IntExp { value = 43; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "1+1" "1+1"
           (OpExp
              {
                oper = PlusOp;
                left = IntExp { value = 1; loc = dummy_loc };
                right = IntExp { value = 1; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "2-3" "2-3"
           (OpExp
              {
                oper = MinusOp;
                left = IntExp { value = 2; loc = dummy_loc };
                right = IntExp { value = 3; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "1+2*3" "1+2*3"
           (OpExp
              {
                oper = PlusOp;
                left = IntExp { value = 1; loc = dummy_loc };
                right =
                  OpExp
                    {
                      oper = TimesOp;
                      left = IntExp { value = 2; loc = dummy_loc };
                      right = IntExp { value = 3; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                loc = dummy_loc;
              });
         test_parser "2/3+2" "2/3+2"
           (OpExp
              {
                oper = PlusOp;
                left =
                  OpExp
                    {
                      oper = DivideOp;
                      left = IntExp { value = 2; loc = dummy_loc };
                      right = IntExp { value = 3; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                right = IntExp { value = 2; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "13 | 1" "13 | 1"
           (OpExp
              {
                oper = OrOp;
                left = IntExp { value = 13; loc = dummy_loc };
                right = IntExp { value = 1; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "1 & 11" "1 & 11"
           (OpExp
              {
                oper = AndOp;
                left = IntExp { value = 1; loc = dummy_loc };
                right = IntExp { value = 11; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "1 = 0" "1 = 0"
           (OpExp
              {
                oper = EqOp;
                left = IntExp { value = 1; loc = dummy_loc };
                right = IntExp { value = 0; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "13 <> 51" "13 <> 51"
           (OpExp
              {
                oper = NeqOp;
                left = IntExp { value = 13; loc = dummy_loc };
                right = IntExp { value = 51; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "1 < 11" "1 < 11"
           (OpExp
              {
                oper = LtOp;
                left = IntExp { value = 1; loc = dummy_loc };
                right = IntExp { value = 11; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "1 <= 0" "1 <= 0"
           (OpExp
              {
                oper = LteOp;
                left = IntExp { value = 1; loc = dummy_loc };
                right = IntExp { value = 0; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "13 > 51" "13 > 51"
           (OpExp
              {
                oper = GtOp;
                left = IntExp { value = 13; loc = dummy_loc };
                right = IntExp { value = 51; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "1 >= 11" "1 >= 11"
           (OpExp
              {
                oper = GteOp;
                left = IntExp { value = 1; loc = dummy_loc };
                right = IntExp { value = 11; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "1 + 1 < 0  & 4<= 0" "1 + 1 < 0  & 4<= 0"
           (OpExp
              {
                oper = AndOp;
                left =
                  OpExp
                    {
                      oper = LtOp;
                      left =
                        OpExp
                          {
                            oper = PlusOp;
                            left = IntExp { value = 1; loc = dummy_loc };
                            right = IntExp { value = 1; loc = dummy_loc };
                            loc = dummy_loc;
                          };
                      right = IntExp { value = 0; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                right =
                  OpExp
                    {
                      oper = LteOp;
                      left = IntExp { value = 4; loc = dummy_loc };
                      right = IntExp { value = 0; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                loc = dummy_loc;
              });
         test_parser "sequencing" "(nil; nil)"
           (SeqExp
              {
                exps =
                  [ NilExp { loc = dummy_loc }; NilExp { loc = dummy_loc } ];
              });
         test_parser "hoge()" "hoge()"
           (CallExp { func = "hoge"; args = []; loc = dummy_loc });
         test_parser "foo_(a)" "foo_(a)"
           (CallExp
              {
                func = "foo_";
                args =
                  [ VarExp { var = SimpleVar { var = "a"; loc = dummy_loc } } ];
                loc = dummy_loc;
              });
         test_parser "bar1(a, 1, b)" "bar1(a, 1, b)"
           (CallExp
              {
                func = "bar1";
                args =
                  [
                    VarExp { var = SimpleVar { var = "a"; loc = dummy_loc } };
                    IntExp { value = 1; loc = dummy_loc };
                    VarExp { var = SimpleVar { var = "b"; loc = dummy_loc } };
                  ];
                loc = dummy_loc;
              });
         test_parser "rcd { x = 1 }" "rcd { x = 1 }"
           (RecordExp
              {
                typ = "rcd";
                fields =
                  [ ("x", IntExp { value = 1; loc = dummy_loc }, dummy_loc) ];
                loc = dummy_loc;
              });
         test_parser "person { name = \"bob\"}" "person { name = \"bob\"}"
           (RecordExp
              {
                typ = "person";
                fields =
                  [
                    ( "name",
                      StringExp { str = "bob"; loc = dummy_loc },
                      dummy_loc );
                  ];
                loc = dummy_loc;
              });
         test_parser "foo { x = 1, y = 3}" "foo { x = 1, y = 3}"
           (RecordExp
              {
                typ = "foo";
                fields =
                  [
                    ("x", IntExp { value = 1; loc = dummy_loc }, dummy_loc);
                    ("y", IntExp { value = 3; loc = dummy_loc }, dummy_loc);
                  ];
                loc = dummy_loc;
              });
         test_parser "a := 100" "a := 100"
           (AssignExp
              {
                var = SimpleVar { var = "a"; loc = dummy_loc };
                exp = IntExp { value = 100; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "array1[100] := 1" "array1[100] := 1"
           (AssignExp
              {
                var =
                  SubscriptVar
                    {
                      var = SimpleVar { var = "array1"; loc = dummy_loc };
                      exp = IntExp { value = 100; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                exp = IntExp { value = 1; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "rcd.x := 1" "rcd.x := 1"
           (AssignExp
              {
                var =
                  FieldVar
                    {
                      var = SimpleVar { var = "rcd"; loc = dummy_loc };
                      mem = "x";
                      loc = dummy_loc;
                    };
                exp = IntExp { value = 1; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "if 1 < 2 then 1 else 2" "if 1 < 2 then 1 else 2"
           (IfExp
              {
                test =
                  OpExp
                    {
                      oper = LtOp;
                      left = IntExp { value = 1; loc = dummy_loc };
                      right = IntExp { value = 2; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                then' = IntExp { value = 1; loc = dummy_loc };
                else' = Some (IntExp { value = 2; loc = dummy_loc });
                loc = dummy_loc;
              });
         test_parser "if 1 + 1 < 2 then 1*3 else 1+2"
           "if 1 + 1 < 2 then 1*3 else 1+2"
           (IfExp
              {
                test =
                  OpExp
                    {
                      oper = LtOp;
                      left =
                        OpExp
                          {
                            oper = PlusOp;
                            left = IntExp { value = 1; loc = dummy_loc };
                            right = IntExp { value = 1; loc = dummy_loc };
                            loc = dummy_loc;
                          };
                      right = IntExp { value = 2; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                then' =
                  OpExp
                    {
                      oper = TimesOp;
                      left = IntExp { value = 1; loc = dummy_loc };
                      right = IntExp { value = 3; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                else' =
                  Some
                    (OpExp
                       {
                         oper = PlusOp;
                         left = IntExp { value = 1; loc = dummy_loc };
                         right = IntExp { value = 2; loc = dummy_loc };
                         loc = dummy_loc;
                       });
                loc = dummy_loc;
              });
         test_parser "if 1 + 1 < 2 then 1*3" "if 1 + 1 < 2 then 1*3"
           (IfExp
              {
                test =
                  OpExp
                    {
                      oper = LtOp;
                      left =
                        OpExp
                          {
                            oper = PlusOp;
                            left = IntExp { value = 1; loc = dummy_loc };
                            right = IntExp { value = 1; loc = dummy_loc };
                            loc = dummy_loc;
                          };
                      right = IntExp { value = 2; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                then' =
                  OpExp
                    {
                      oper = TimesOp;
                      left = IntExp { value = 1; loc = dummy_loc };
                      right = IntExp { value = 3; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                else' = None;
                loc = dummy_loc;
              });
         test_parser "if 1 < 2 then if 3 < 4 then 5 else 6"
           "if 1 < 2 then if 3 < 4 then 5 else 6"
           (IfExp
              {
                test =
                  OpExp
                    {
                      oper = LtOp;
                      left = IntExp { value = 1; loc = dummy_loc };
                      right = IntExp { value = 2; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                then' =
                  IfExp
                    {
                      test =
                        OpExp
                          {
                            oper = LtOp;
                            left = IntExp { value = 3; loc = dummy_loc };
                            right = IntExp { value = 4; loc = dummy_loc };
                            loc = dummy_loc;
                          };
                      then' = IntExp { value = 5; loc = dummy_loc };
                      else' = Some (IntExp { value = 6; loc = dummy_loc });
                      loc = dummy_loc;
                    };
                else' = None;
                loc = dummy_loc;
              });
         test_parser "if 1 < 2 then if 3 < 4 then 5 else 6 else 7"
           "if 1 < 2 then if 3 < 4 then 5 else 6 else 7"
           (IfExp
              {
                test =
                  OpExp
                    {
                      oper = LtOp;
                      left = IntExp { value = 1; loc = dummy_loc };
                      right = IntExp { value = 2; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                then' =
                  IfExp
                    {
                      test =
                        OpExp
                          {
                            oper = LtOp;
                            left = IntExp { value = 3; loc = dummy_loc };
                            right = IntExp { value = 4; loc = dummy_loc };
                            loc = dummy_loc;
                          };
                      then' = IntExp { value = 5; loc = dummy_loc };
                      else' = Some (IntExp { value = 6; loc = dummy_loc });
                      loc = dummy_loc;
                    };
                else' = Some (IntExp { value = 7; loc = dummy_loc });
                loc = dummy_loc;
              });
         test_parser "a" "a"
           (VarExp { var = SimpleVar { var = "a"; loc = dummy_loc } });
         test_parser "rcd.x" "rcd.x"
           (VarExp
              {
                var =
                  FieldVar
                    {
                      var = SimpleVar { var = "rcd"; loc = dummy_loc };
                      mem = "x";
                      loc = dummy_loc;
                    };
              });
         test_parser "array1[100]" "array1[100]"
           (VarExp
              {
                var =
                  SubscriptVar
                    {
                      var = SimpleVar { var = "array1"; loc = dummy_loc };
                      exp = IntExp { value = 100; loc = dummy_loc };
                      loc = dummy_loc;
                    };
              });
         test_parser "while 10 < 100 do 0" "while 10 < 100 do 0"
           (WhileExp
              {
                test =
                  OpExp
                    {
                      oper = LtOp;
                      left = IntExp { value = 10; loc = dummy_loc };
                      right = IntExp { value = 100; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                body = IntExp { value = 0; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "while 1 < 2 do 100 + 500" "while 1 < 2 do 100 + 500"
           (WhileExp
              {
                test =
                  OpExp
                    {
                      oper = LtOp;
                      left = IntExp { value = 1; loc = dummy_loc };
                      right = IntExp { value = 2; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                body =
                  OpExp
                    {
                      oper = PlusOp;
                      left = IntExp { value = 100; loc = dummy_loc };
                      right = IntExp { value = 500; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                loc = dummy_loc;
              });
         test_parser "while 1 < 100 | x > 100 do 1"
           "while 1 < 100 | x > 100 do 1"
           (WhileExp
              {
                test =
                  OpExp
                    {
                      oper = OrOp;
                      left =
                        OpExp
                          {
                            oper = LtOp;
                            left = IntExp { value = 1; loc = dummy_loc };
                            right = IntExp { value = 100; loc = dummy_loc };
                            loc = dummy_loc;
                          };
                      right =
                        OpExp
                          {
                            oper = GtOp;
                            left =
                              VarExp
                                {
                                  var = SimpleVar { var = "x"; loc = dummy_loc };
                                };
                            right = IntExp { value = 100; loc = dummy_loc };
                            loc = dummy_loc;
                          };
                      loc = dummy_loc;
                    };
                body = IntExp { value = 1; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "for x := 1 to 2 do 3" "for x := 1 to 2 do 3"
           (ForExp
              {
                var = "x";
                escape = ref true;
                lo = IntExp { value = 1; loc = dummy_loc };
                hi = IntExp { value = 2; loc = dummy_loc };
                body = IntExp { value = 3; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "for x := 1 to 200 do (1;1)" "for x := 1 to 2 do (1;1)"
           (ForExp
              {
                var = "x";
                escape = ref true;
                lo = IntExp { value = 1; loc = dummy_loc };
                hi = IntExp { value = 2; loc = dummy_loc };
                body =
                  SeqExp
                    {
                      exps =
                        [
                          IntExp { value = 1; loc = dummy_loc };
                          IntExp { value = 1; loc = dummy_loc };
                        ];
                    };
                loc = dummy_loc;
              });
         test_parser "for x := 1+1 to a[100] do 1" "for x := 1+1 to a[100] do 1"
           (ForExp
              {
                var = "x";
                escape = ref true;
                lo =
                  OpExp
                    {
                      oper = PlusOp;
                      left = IntExp { value = 1; loc = dummy_loc };
                      right = IntExp { value = 1; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                hi =
                  VarExp
                    {
                      var =
                        SubscriptVar
                          {
                            var = SimpleVar { var = "a"; loc = dummy_loc };
                            exp = IntExp { value = 100; loc = dummy_loc };
                            loc = dummy_loc;
                          };
                    };
                body = IntExp { value = 1; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "break" "break" (BreakExp { loc = dummy_loc });
         test_parser "intArray[100] of 0" "intArray[100] of 0"
           (ArrayExp
              {
                typ = "intArray";
                size = IntExp { value = 100; loc = dummy_loc };
                init = IntExp { value = 0; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "hoge[N] of 0" "hoge[N] of 0"
           (ArrayExp
              {
                typ = "hoge";
                size = VarExp { var = SimpleVar { var = "N"; loc = dummy_loc } };
                init = IntExp { value = 0; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "monad[yy] of \"str\"" "monad[yy] of \"str\""
           (ArrayExp
              {
                typ = "monad";
                size =
                  VarExp { var = SimpleVar { var = "yy"; loc = dummy_loc } };
                init = StringExp { str = "str"; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "let function foo() = 1 in 3+2 end"
           "let function foo() = 1 in 3+2 end"
           (LetExp
              {
                decs =
                  [
                    FunctionDec
                      [
                        {
                          fname = "foo";
                          fparams = [];
                          fresult = None;
                          fbody = IntExp { value = 1; loc = dummy_loc };
                          floc = dummy_loc;
                        };
                      ];
                  ];
                body =
                  OpExp
                    {
                      oper = PlusOp;
                      left = IntExp { value = 3; loc = dummy_loc };
                      right = IntExp { value = 2; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                loc = dummy_loc;
              });
         test_parser "let function bar(x : int) : int = 1 in 3+2 end"
           "let function bar(x : int) : int = 1 in 3+2 end"
           (LetExp
              {
                decs =
                  [
                    FunctionDec
                      [
                        {
                          fname = "bar";
                          fparams =
                            [
                              {
                                name = "x";
                                escape = ref true;
                                typ = "int";
                                loc = dummy_loc;
                              };
                            ];
                          fresult = Some ("int", dummy_loc);
                          fbody = IntExp { value = 1; loc = dummy_loc };
                          floc = dummy_loc;
                        };
                      ];
                  ];
                body =
                  OpExp
                    {
                      oper = PlusOp;
                      left = IntExp { value = 3; loc = dummy_loc };
                      right = IntExp { value = 2; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                loc = dummy_loc;
              });
         test_parser "let type intList = list in 1+1 end"
           "let type intList = list in 1+1 end"
           (LetExp
              {
                decs =
                  [
                    TypeDec
                      [
                        {
                          tname = "intList";
                          tty = NameTy { name = "list"; loc = dummy_loc };
                          tloc = dummy_loc;
                        };
                      ];
                  ];
                body =
                  OpExp
                    {
                      oper = PlusOp;
                      left = IntExp { value = 1; loc = dummy_loc };
                      right = IntExp { value = 1; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                loc = dummy_loc;
              });
         test_parser "let type = {x : int, y : int} in 1+2 end"
           "let type pair = {x : int, y : int} in 1+2 end"
           (LetExp
              {
                decs =
                  [
                    TypeDec
                      [
                        {
                          tname = "pair";
                          tty =
                            RecordTy
                              {
                                fields =
                                  [
                                    {
                                      name = "x";
                                      escape = ref true;
                                      typ = "int";
                                      loc = dummy_loc;
                                    };
                                    {
                                      name = "y";
                                      escape = ref true;
                                      typ = "int";
                                      loc = dummy_loc;
                                    };
                                  ];
                              };
                          tloc = dummy_loc;
                        };
                      ];
                  ];
                body =
                  OpExp
                    {
                      oper = PlusOp;
                      left = IntExp { value = 1; loc = dummy_loc };
                      right = IntExp { value = 2; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                loc = dummy_loc;
              });
         test_parser "let type a = array of string in 100 end"
           "let type a = array of string in 100 end"
           (LetExp
              {
                decs =
                  [
                    TypeDec
                      [
                        {
                          tname = "a";
                          tty = ArrayTy { name = "string"; loc = dummy_loc };
                          tloc = dummy_loc;
                        };
                      ];
                  ];
                body = IntExp { value = 100; loc = dummy_loc };
                loc = dummy_loc;
              });
         test_parser "let var foo := 113 in 3+2 end"
           "let var foo := 113 in 3+2 end"
           (LetExp
              {
                decs =
                  [
                    VarDec
                      {
                        name = "foo";
                        escape = ref true;
                        typ = None;
                        init = IntExp { value = 113; loc = dummy_loc };
                        loc = dummy_loc;
                      };
                  ];
                body =
                  OpExp
                    {
                      oper = PlusOp;
                      left = IntExp { value = 3; loc = dummy_loc };
                      right = IntExp { value = 2; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                loc = dummy_loc;
              });
         test_parser "let var foo : int := 113 in 3+2"
           "let var foo : int := 113 in 3+2 end"
           (LetExp
              {
                decs =
                  [
                    VarDec
                      {
                        name = "foo";
                        escape = ref true;
                        typ = Some ("int", dummy_loc);
                        init = IntExp { value = 113; loc = dummy_loc };
                        loc = dummy_loc;
                      };
                  ];
                body =
                  OpExp
                    {
                      oper = PlusOp;
                      left = IntExp { value = 3; loc = dummy_loc };
                      right = IntExp { value = 2; loc = dummy_loc };
                      loc = dummy_loc;
                    };
                loc = dummy_loc;
              });
       ]

let () = run_test_tt_main test_parser
