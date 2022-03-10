%{
  open Ast
  open Parsing
  open Symbol
%}

%token TYPE
%token VAR
%token FUNCTION
%token BREAK
%token OF
%token END
%token IN
%token NIL
%token LET
%token DO
%token TO
%token FOR
%token WHILE
%token ELSE
%token THEN
%token IF
%token ARRAY
%token ASSIGN
%token OR
%token AND
%token GE
%token GT
%token LE
%token LT
%token NEQ
%token EQ
%token DIVIDE
%token TIMES
%token MINUS
%token PLUS
%token DOT
%token RBRACE
%token LBRACE
%token RBRACK
%token LBRACK
%token RPAREN
%token LPAREN
%token SEMICOLON
%token COLON
%token COMMA
%token <string> STRING
%token <int> INT
%token <string> ID
%token EOF

%right ASSIGN
%nonassoc THEN 
%nonassoc ELSE DO OF
%left OR
%left AND
%left EQ NEQ
%left GE GT LE LT
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%type <Ast.dec list> prog
%start prog

%% 

prog: decs EOF { $1 }

decs:
  | tydec+ non_tydecs { (TypeDec $1) :: $2 } 
  | vardec any_decs { $1 :: $2 }
  | fundec+ non_fundecs { (FunctionDec $1) :: $2 }
  ;

any_decs:
  | { [] }
  | tydec+ non_tydecs { (TypeDec $1) :: $2 } 
  | vardec any_decs { $1 :: $2 }
  | fundec+ non_fundecs { (FunctionDec $1) :: $2 }
  ;

non_fundecs:
  | { [] }
  | vardec any_decs { $1 :: $2 }
  | tydec+ non_tydecs { (TypeDec $1) :: $2 }
  ;
  
non_tydecs:
  | { [] }
  | vardec any_decs { $1 :: $2 }
  | fundec+ non_fundecs { (FunctionDec $1) :: $2 }
  ;

vardec:
  | VAR; name = ID; ASSIGN; init = exp
    { VarDec ({ name = symbol name; escape = ref true; typ = None; init; pos = 0; }) }
  ;

tydec: TYPE; id = ID; EQ; ty = ty 
  { { name = symbol id; ty; pos = 0; } }

ty:
  | id = ID 
    { NameTy (symbol id, 0) }
  | LBRACE; tyfields = tyfields; RBRACE 
    { RecordTy tyfields }
  | ARRAY; OF; id = ID 
    { ArrayTy (symbol id, 0) }
  

tyfields: fields = separated_list(COMMA, tyfield) 
  { fields }

tyfield:
  | name = ID; COLON; typ = ID
    {
      let field: field = {
        name = symbol name;
        escape = ref true;
        typ = symbol typ;
        pos = 0;
      } in field
    }
  ;

fundec:
  | FUNCTION; id = ID; LPAREN; params = tyfields; result = annotation?; RPAREN; EQ; int = INT
    { 
      {
        name = symbol id;
        params;
        result;
        body = IntExp int;
        pos = 0;
      } 
    }
  ;

annotation: COLON; id = ID 
  { (symbol id, 0) }

exp:
  | NIL 
    { NilExp }
  | int = INT 
    { IntExp int }
  | string = STRING 
    { StringExp (string, 0) }
  | LPAREN; exps = separated_list(SEMICOLON, exp); RPAREN
    { SeqExp (List.map (fun exp -> (exp, 0)) exps) }
  | var = lvalue; ASSIGN; exp = exp 
    { AssignExp { var; exp; pos = 0 } }
  | left = exp PLUS right = exp 
    { OpExp { left; oper = PlusOp; right; pos = 0 } }
  | left = exp MINUS right = exp 
    { OpExp { left; oper = MinusOp; right; pos = 0 } }
  | MINUS right = exp
    { OpExp { left = IntExp 0; oper = MinusOp; right; pos = 0 }} %prec UMINUS
  | left = exp TIMES right = exp 
    { OpExp { left; oper = TimesOp; right; pos = 0 } } 
  | left = exp DIVIDE right = exp 
    { OpExp { left; oper = DivideOp; right; pos = 0 } }
  | left = exp AND right = exp 
    { OpExp { left; oper = AndOp; right; pos = 0 } }
  | left = exp OR right = exp 
    { OpExp { left; oper = OrOp; right; pos = 0 } }
  | left = exp GE right = exp 
    { OpExp { left; oper = GeOp; right; pos = 0 } } 
  | left = exp GT right = exp 
    { OpExp { left; oper = GtOp; right; pos = 0 } }
  | left = exp LE right = exp 
    { OpExp { left; oper = LeOp; right; pos = 0 } }
  | left = exp LT right = exp 
    { OpExp { left; oper = LtOp; right; pos = 0 } }
  | left = exp EQ right = exp 
    { OpExp { left; oper = EqOp; right; pos = 0 } } 
  | left = exp NEQ right = exp 
    { OpExp { left; oper = NeqOp; right; pos = 0 } }
  | IF test = exp THEN exp1 = exp
    { IfExp { test; then' = exp1; else' = None; pos = 0 } }
  | IF test = exp THEN exp1 = exp ELSE exp2 = exp
    { IfExp { test; then' = exp1; else' = Some exp2; pos = 0 } }
  | WHILE test = exp THEN body = exp
    { WhileExp { test; body; pos = 0 } }
  | FOR var = ID ASSIGN lo = exp TO hi = exp DO body = exp
    { ForExp { var = symbol var; escape = ref true; lo; hi; body; pos = 0 } }
  | BREAK
    { BreakExp 0 }
  | LET decs = decs IN exps = separated_list(SEMICOLON, exp) END
    { LetExp { decs; body = SeqExp (List.map (fun exp -> (exp, 0)) exps); pos = 0  } }
  | typ = ID LBRACK size = exp RBRACK OF init = exp
    { ArrayExp { typ = symbol typ; size; init; pos = 0 } }
  | lvalue = lvalue 
    { VarExp lvalue }
  ;

lvalue:
  | id = ID 
    { SimpleVar (symbol id, 0) }
  | lvalue_not_id = lvalue_not_id 
    { lvalue_not_id }

lvalue_not_id:
  | lvalue = lvalue DOT id = ID 
    { FieldVar (lvalue, symbol id, 0) }
  | id = ID LBRACK exp = exp RBRACK 
    { SubscriptVar (SimpleVar (symbol id, 0), exp, 0) }
  | lvalue = lvalue_not_id LBRACK exp = exp RBRACK 
    { SubscriptVar (lvalue, exp, 0) }

  