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
    | eof           { EOF }
    | "while"       { WHILE }
    | "for"         { FOR }
    | "to"          { TO }
    | "break"       { BREAK }
    | "let"         { LET }
    | "in"          { IN }
    | "end"         { END }
    | "function"    { FUNCTION }
    | "var"         { VAR }
    | "type"        { TYPE }
    | "array"       { ARRAY }
    | "if"          { IF }
    | "then"        { THEN }
    | "else"        { ELSE }
    | "do"          { DO }
    | "of"          { OF }
    | "nil"         { NIL }
    | ","           { COMMA }
    | ":"           { COLON }
    | ";"           { SEMICOLON }
    | "("           { LPAREN }
    | ")"           { RPAREN }
    | "["           { LBRACKET }
    | "]"           { RBRACKET }
    | "{"           { LBRACE }
    | "}"           { RBRACE }
    | "."           { DOT }
    | "+"           { PLUS }
    | "-"           { MINUS }
    | "*"           { TIMES }
    | "/"           { DIV }
    | "<>"          { NEQ }
    | "<"           { LT }
    | "<="          { LTE }
    | ">"           { GT }
    | ">="          { GTE }
    | "&"           { AND }
    | "|"           { OR }
    | ":="          { ASSIGN } 
    | litint as num { INT (int_of_string num) }
    | ident as id   { ID id }
    | "\"" _* "\"" as str { let s = String.sub str 1 ((String.length str) - 2) in STR(s) }
    | "/*" _* "*/"        { token lexbuf } 
    | _* as str { STR(str) }