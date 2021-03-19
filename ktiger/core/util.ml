open Lexer
let rec string_tokenizer_aux buf =
  match Lexer.token buf with
  | EOF -> [ EOF ]
  | tok -> tok :: string_tokenizer_aux buf

let string_tokenizer str =
  let lexbuf = Lexing.from_string str in
  string_tokenizer_aux lexbuf

let show_token_list (toks : token list) = String.concat ", " (List.map Lexer.show_token toks)




