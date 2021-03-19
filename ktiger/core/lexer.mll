{   
    type token = [%import: Parser.token] [@@deriving show, eq]
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' 'A'-'Z']+ (alpha | digit | '_')*
let litint = digit+
let whitspaces = [' ' '\t' '\n' '\r']

rule token = parse
    | whitspaces+ { token lexbuf }
    | "while"    { WHILE }
    | "for"      { FOR }
    | "to"       { TO }
    | "break"    { BREAK }
    | "let"      { LET }
    | "in"       { IN }
    | "end"      { END }
    | "function" { FUNCTION }
    | "var"      { VAR }
    | "type"     { TYPE }
    | "array"    { ARRAY }
    | "if"       { IF }
    | "then"     { THEN }
    | "else"     { ELSE }
    | "do"       { DO }
    | "of"       { OF }
    | "nil"      { NIL }
    | "("        { LPAREN }
    | eof { EOF }