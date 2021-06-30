{   
    type token = [%import: Parser.token] [@@deriving show, eq]
    exception Error of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' 'A'-'Z']+ (alpha | digit | '_')*
let litint = digit+
let whitspaces = [' ' '\t' '\n' '\r']

rule token = parse
    | whitspaces+ { token lexbuf }
    | eof                   { EOF }
    | "while"               { WHILE }
    | "for"                 { FOR }
    | "to"                  { TO }
    | "break"               { BREAK }
    | "let"                 { LET }
    | "in"                  { IN }
    | "end"                 { END }
    | "function"            { FUNCTION }
    | "var"                 { VAR }
    | "type"                { TYPE }
    | "array"               { ARRAY }
    | "if"                  { IF }
    | "then"                { THEN }
    | "else"                { ELSE }
    | "do"                  { DO }
    | "of"                  { OF }
    | "nil"                 { NIL }
    | ","                   { COMMA }
    | ":"                   { COLON }
    | ";"                   { SEMICOLON }
    | "("                   { LPAREN }
    | ")"                   { RPAREN }
    | "["                   { LBRACKET }
    | "]"                   { RBRACKET }
    | "{"                   { LBRACE }
    | "}"                   { RBRACE }
    | "."                   { DOT }
    | "+"                   { PLUS }
    | "-"                   { MINUS }
    | "*"                   { TIMES }
    | "/"                   { DIV }
    | "="                   { EQ }
    | "<>"                  { NEQ }
    | "<"                   { LT }
    | "<="                  { LTE }
    | ">"                   { GT }
    | ">="                  { GTE }
    | "&"                   { AND }
    | "|"                   { OR }
    | ":="                  { ASSIGN } 
    | litint as num         { INT (int_of_string num) }
    | ident as id           { ID id }
    | "\""                  { lex_string (Buffer.create 16) lexbuf }
    | "/*" _* "*/"          { token lexbuf } 
    | _ as c                { raise (Error (Printf.sprintf "At offset %d: unexpected character %c.\n" (Lexing.lexeme_start lexbuf) c)) }

and lex_string buf = parse
    | "\""   { STR (Buffer.contents buf) }
    | _ as c { Buffer.add_char buf c; lex_string buf lexbuf }
    