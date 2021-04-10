%{ 
      open Absyn
      open Location
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
%token LOWEST

%start <exp> program

%nonassoc FUNCTION OF THEN DO
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
      | e = exp EOF { e }

exp:
  | "nil"                                                               { NilExp { loc = to_location($startpos)} }
  | i = INT                                                             { IntExp { value = i; loc = to_location($startpos) } }
  | s = STR                                                             { StringExp { str = s; loc = to_location($startpos) } }
  | "-" e = exp %prec UMINUS                                            { OpExp { oper = MinusOp; left = IntExp { value = 0; loc = to_location($startpos) }; right = e ; loc = to_location($startpos) } }
  | left = exp oper = binop right = exp                                 { OpExp { oper = oper; left = left; right = right; loc = to_location($startpos)} }
  | "(" exps = separated_list(";", exp) ")"                             { SeqExp { exps = exps } }
  | func = ID "(" args = separated_list(",", exp) ")"                   { CallExp { func = func; args = args; loc = to_location($startpos)} }
  | lv = lvalue                                                         { VarExp { var = lv } } 
  | id = ID "{" fields = separated_list(",", field) "}"                 { RecordExp { typ = id; fields = fields;  loc = to_location($startpos) } }
  | lv = lvalue ":=" e = exp                                            { AssignExp { var = lv; exp = e; loc = to_location($startpos)} }
  | "if" test = exp "then" e1 = exp "else" e2 = exp                     { IfExp { test = test; then' = e1; else' = (Some e2); loc = to_location($startpos) } } 
  | "if" test = exp "then" e = exp                                      { IfExp { test = test; then' = e; else' = None; loc = to_location($startpos) }  }
  | "while" test = exp "do" body = exp                                  { WhileExp { test = test; body = body; loc = to_location($startpos) }}
  | "for" id = ID ":=" lo = exp "to" hi = exp "do" body = exp           { ForExp { var = id; escape = ref true; lo = lo; hi = hi; body = body; loc = to_location($startpos) } }
  | BREAK                                                               { BreakExp { loc = to_location($startpos) } }
  | lv = lvalue "[" size = exp "]" "of" init = exp                      { match lv with
                                                                           | SimpleVar { var = var; loc = loc} -> ArrayExp { typ = var; size = size; init = init; loc = loc } 
                                                                           | _ -> BreakExp { loc = to_location($startpos)} }
  | "let" decs = list(dec) "in" body = exp "end"                        { LetExp { decs = decs; body = body; loc = to_location($startpos) } }



dec:  
      | fds = separated_nonempty_list(";", fundec)     { FunctionDec fds } 
      | tds = separated_nonempty_list(";", tydec)      { TypeDec tds } 
      | vd = vardec                                    { vd } 



fundec:
      | "function" fname = ID "(" fparams = separated_list(",", tyfield) ")" "=" fbody = exp                      { { fname = fname; fparams = fparams; fresult = None; fbody = fbody; floc = to_location($startpos) } }
      | "function" fname = ID "(" fparams = separated_list(",", tyfield) ")" ":" fresult = ID "=" fbody = exp     { let loc = to_location($startpos) in { fname = fname; fparams = fparams; fresult = Some((fresult, loc)); fbody = fbody; floc = loc } }


tydec:
      | "type" typ = ID "=" ty = ty {  { tname = typ; tty = ty; tloc = to_location($startpos) } }


vardec:
      | "var" var = ID ":=" init = exp                 { let loc = to_location($startpos) in VarDec { name = var; escape = ref true; typ = None; init = init; loc = loc } }
      | "var" var = ID ":" typ = ID ":=" init = exp    { let loc = to_location($startpos) in VarDec { name = var; escape = ref true; typ = Some (typ, loc); init = init; loc = loc } }


ty:
      | typ = ID                                             { NameTy { name = typ; loc = to_location($startpos)} }
      | "{" tyfields = separated_list(",", tyfield) "}"      { RecordTy { fields = tyfields } }
      | "array" "of" typ = ID                                { ArrayTy { name = typ; loc = to_location($startpos)} }

tyfield:
      | name = ID ":" typ = ID       { { name = name; escape = ref true; typ = typ; loc = to_location($startpos) } }



field:
      | id = ID "=" e = exp      { (id, e, to_location($startpos)) }
 
lvalue:
      | id = ID                           { SimpleVar { var = id; loc = to_location($startpos) } }
      | lv = lvalue "." mem = ID          { FieldVar { var = lv; mem = mem; loc = to_location($startpos) } }
      | lv = lvalue "[" e = exp "]"       { SubscriptVar { var = lv; exp = e; loc = to_location($startpos) } }


%inline binop:
  | PLUS    { PlusOp }
  | MINUS   { MinusOp }
  | TIMES   { TimesOp }
  | DIV     { DivideOp }
  | OR      { OrOp }
  | AND     { AndOp }
  | EQ      { EqOp }
  | NEQ     { NeqOp }
  | LT      { LtOp }
  | LTE     { LteOp } 
  | GT      { GtOp }
  | GTE     { GteOp }

