%{ 

%}

%token WHILE "while" FOR "for"
%token TO "to"
%token BREAK "break"
%token LET "let" IN "in" END "end"
%token FUNCTION "function"
%token VAR "var"
%token TYPE "type"
%token ARRAY "array"
%token IF "if" THEN "then" ELSE "else"
%token DO "do"
%token OF "of"
%token NIL "nil"

%token COMMA ","
%token COLON ":"
%token SEMICOLON ";"
%token LPAREN "(" RPAREN ")"
%token LBRACKET "[" RBRACKET "]"
%token LBRACE "{" RBRACE "}"
%token DOT "."
%token PLUS "+" MINUS "-" TIMES "*" DIV "/"
%token EQ "=" NEQ "<>" LT "<" LTE "<=" GT ">" GTE ">=" AND "&" OR "|"
%token ASSIGN ":="
%token EOF

%token <string> ID
%token <int> INT
%token <string> STR
%token UMINUS

%start <unit> program


%nonassoc OF THEN DO
%nonassoc ELSE
%nonassoc ASSIGN 

%left OR 
%left AND
%nonassoc EQ NEQ LT LTE GT GTE
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

%%

program:
      | exp EOF {  }

exp:
  | lvalue                                                  {  } 
  | "nil"                                                   {  }
  | "(" separated_list(";", exp) ")"                        {  }
  | INT                                                     {  }
  | STR                                                     {  }
  | "-" exp %prec UMINUS                                    {  }
  | exp binop exp                                           {  }
  | ID "(" separated_list(",", exp) ")"                     {  }
  | ID "{" separated_list(",", field) "}"                   {  }
  | lvalue "[" exp "]" "of" exp                             {  }
  | lvalue ":=" exp                                         {  }
  | "if" exp "then" exp "else" exp                          {  } 
  | "if" exp "then" exp                                     {  }
  | "while" exp "do" exp                                    {  }
  | "for" ID ":=" exp "to" exp "do" exp                     {  }
  | BREAK                                                   {  }
  | "let" list(dec) "in" separated_list(";", exp) "end"     {  }

dec:
      | tydec     { }
      | vardec    { }
      | fundec    { }

tydec:
      | "type" ID "=" ty { }

ty:
      | ID                                      {  }
      | "{" separated_list(",", tyfield) "}"    {  }
      | "array" "of" ID                         {  }

tyfield:
      | ID ":" ID       {  }

vardec:
      | "var" ID ":=" exp           {  }
      | "var" ID ":" ID ":=" exp    {  }

fundec:
      | "function" ID "(" separated_list(",", tyfield) ")" "=" exp            {  }
      | "function" ID "(" separated_list(",", tyfield) ")" ":" ID "=" exp     {  }


field:
      | ID "=" exp      { }
      
lvalue:
      | ID {  }
      | lvalue "." ID         {  }
      | lvalue "[" exp "]"    {  }


%inline binop:
  | PLUS    {  }
  | MINUS   {  }
  | TIMES   {  }
  | DIV     {  }
  | OR      {  }
  | AND     {  }
  | EQ      {  }
  | NEQ     {  }
  | LT      {  }
  | LTE     {  } 
  | GT      {  }
  | GTE     {  }

