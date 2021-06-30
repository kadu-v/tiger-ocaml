open Core.Lexer
open Core.Absyn
open Core.Util

let lexbuf = Lexing.from_string "let function foo () = 1 in 1"

let toks = string_tokenizer "if 1 > 0 then \"x\" else \"y\""

let () = print_endline (String.concat "," (List.map show_token toks))

let () = print_endline "Hello, World!"

let ast = string_parser "if 1 > 0 then \"x\" else \"y\""

let () = print_endline (show_exp ast)

let f = show_exp
