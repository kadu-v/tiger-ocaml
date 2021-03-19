let rec string_tokenizer_aux buf =
  match Lexer.token buf with
  | EOF -> [ Parser.EOF ]
  | tok -> tok :: string_tokenizer_aux buf

let string_tokenizer str =
  let lexbuf = Lexing.from_string str in
  string_tokenizer_aux lexbuf