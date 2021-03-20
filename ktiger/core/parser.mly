%{ 

%}

%token WHILE FOR
%token TO
%token BREAK
%token LET IN END
%token FUNCTION
%token VAR
%token TYPE
%token ARRAY
%token IF THEN ELSE
%token DO
%token OF
%token NIL

%token COMMA
%token COLON
%token SEMICOLON
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE
%token DOT
%token PLUS MINUS TIMES DIV
%token EQ NEQ LT LTE GT GTE AND OR
%token ASSIGN
%token EOF

%token <string> ID
%token <int> INT
%token <string> STR

%start <unit> program

%%

program:
      | exp EOF {  }

exp:
  | LPAREN {  }