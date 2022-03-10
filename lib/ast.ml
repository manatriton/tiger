open Core

type token =
  | WHILE
  | VAR
  | TYPE
  | TO
  | TIMES
  | THEN
  | STRING of string
  | SEMICOLON
  | RPAREN
  | RBRACK
  | RBRACE
  | PLUS
  | OR
  | OF
  | NIL
  | NEQ
  | MINUS
  | LT
  | LPAREN
  | LET
  | LE
  | LBRACK
  | LBRACE
  | INT of int
  | IN
  | IF
  | ID of string
  | GT
  | GE
  | FUNCTION
  | FOR
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | DO
  | DIVIDE
  | COMMA
  | COLON
  | BREAK
  | ASSIGN
  | ARRAY
  | AND
[@@deriving sexp]

type pos = int [@@deriving sexp]

type var =
  | SimpleVar of Symbol.t * pos
  | FieldVar of var * Symbol.t * pos
  | SubscriptVar of var * exp * pos
[@@deriving sexp]

and exp =
  | VarExp of var
  | NilExp
  | IntExp of int
  | StringExp of string * pos
  | CallExp of { func : Symbol.t; args : exp list; pos : pos }
  | OpExp of { left : exp; oper : oper; right : exp; pos : pos }
  | RecordExp of {
      fields : (Symbol.t * exp * pos) list;
      typ : Symbol.t;
      pos : pos;
    }
  | SeqExp of (exp * pos) list
  | AssignExp of { var : var; exp : exp; pos : pos }
  | IfExp of { test : exp; then' : exp; else' : exp option; pos : pos }
  | WhileExp of { test : exp; body : exp; pos : pos }
  | ForExp of {
      var : Symbol.t;
      escape : bool ref;
      lo : exp;
      hi : exp;
      body : exp;
      pos : pos;
    }
  | BreakExp of pos
  | LetExp of { decs : dec list; body : exp; pos : pos }
  | ArrayExp of { typ : Symbol.t; size : exp; init : exp; pos : pos }
[@@deriving sexp]

and dec =
  | FunctionDec of fundec list
  | VarDec of {
      name : Symbol.t;
      escape : bool ref;
      typ : (Symbol.t * pos) option;
      init : exp;
      pos : pos;
    }
  | TypeDec of type_info list
[@@deriving sexp]

and ty =
  | NameTy of Symbol.t * pos
  | RecordTy of field list
  | ArrayTy of Symbol.t * pos
[@@deriving sexp]

and type_info = { name : Symbol.t; ty : ty; pos : pos } [@@deriving sexp]

and oper =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
  | AndOp
  | OrOp
[@@deriving sexp]

and field = { name : Symbol.t; escape : bool ref; typ : Symbol.t; pos : pos }
[@@deriving sexp]

and fundec = {
  name : Symbol.t;
  params : field list;
  result : (Symbol.t * pos) option;
  body : exp;
  pos : pos;
}
[@@deriving sexp]
