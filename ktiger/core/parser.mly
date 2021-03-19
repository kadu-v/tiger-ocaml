%{ 

%}

%token WHILE
%token FOR
%token TO
%token BREAK
%token LET
%token IN
%token END
%token FUNCTION
%token VAR
%token TYPE
%token ARRAY
%token IF
%token THEN
%token ELSE
%token DO
%token OF
%token NIL
%token LPAREN
%token EOF

%start <unit> program

%%

program:
      | exp EOF {  }

exp:
  | LPAREN {  }