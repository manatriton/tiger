open Ast
open! Core
open Semant

let venv = Env.base_venv
let tenv = Env.base_tenv
let trans_exp = Semant.trans_exp venv tenv

let ast_exp_of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let failwith_error error =
  let s =
    match error with
    | Wrong_num_arguments -> "wrong number of arguments"
    | Expr_type_clash (expect_ty, actual_ty) ->
        Format.sprintf "expected type %s, got type %s"
          (Sexp.to_string (Types.sexp_of_ty expect_ty))
          (Sexp.to_string (Types.sexp_of_ty actual_ty))
    | Unbound_type ty -> Format.sprintf "unbound type %s" ty
    | Unbound_value v -> Format.sprintf "unbound value %s" v
    | Unexpected_break -> "unexpected break"
  in
  failwith s

let%test "nil_exp" =
  let prog = "nil" in
  match trans_exp (ast_exp_of_string prog) with
  | { exp = (); ty = Types.Nil } -> true
  | _ -> failwith "expected nil"

let%test "int_exp" =
  let prog = "3" in
  match trans_exp (ast_exp_of_string prog) with
  | { exp = (); ty = Types.Int } -> true
  | _ -> failwith "expected int"

let%test "string_exp" =
  match trans_exp (ast_exp_of_string "\"hello\"") with
  | { exp = (); ty = Types.String } -> true
  | _ -> false

let%test "seq_exp_empty" =
  let prog = {|
()
|} in
  match trans_exp (ast_exp_of_string prog) with
  | { exp = (); ty = Types.Unit } -> true
  | _ -> false

let%test "call_exp_print" =
  let prog = {|

print("Hello, world!")

|} in
  match trans_exp (ast_exp_of_string prog) with
  | { exp = (); ty = Types.Unit } -> true
  | _ -> false

let%test "call_exp_flush" =
  let prog = {|

flush()
  
  |} in
  match trans_exp (ast_exp_of_string prog) with
  | { exp = (); ty = Types.Unit } -> true
  | _ -> false

let%test "seq_exp_multi" =
  let prog =
    {|

(
  3;
  "hello";
  4;
  print("hello, world");
  "world"
)
  
|}
  in
  match trans_exp (ast_exp_of_string prog) with
  | { exp = (); ty = Types.String } -> true
  | _ -> false

let%test "assign_exp" =
  let var = SimpleVar (Symbol.symbol "x", 0) in
  let exp = IntExp 0 in
  match Semant.trans_exp venv tenv (AssignExp { var; exp; pos = 0 }) with
  | { exp = (); ty = Types.Unit } -> true
  | _ -> false

let%test "if_exp_no_else" =
  let prog = {|

if 1 then print("Hello, world!")

|} in
  match trans_exp (ast_exp_of_string prog) with
  | { exp = (); ty = Types.Unit } -> true
  | _ -> false

let%test "if_exp_else" =
  let prog = {|

if 1 + 1 then 2 else 4

|} in
  match trans_exp (ast_exp_of_string prog) with
  | { exp = (); ty = Types.Int } -> true
  | _ -> false

let%test "while_exp" =
  let prog = {|
  
while 1 do print("Hello, world!")
  
|} in
  match trans_exp (ast_exp_of_string prog) with
  | { exp = (); ty = Types.Unit } -> true
  | _ -> false

let%test "recursive_function" =
  try
    let prog =
      {|
/* define a recursive function */
let

/* calculate n! */
function nfactor(n: int): int =
		if  n = 0 
			then 1
			else n * nfactor(n-1)

in
	nfactor(10)
end 
|}
    in
    match trans_exp (ast_exp_of_string prog) with
    | { exp = (); ty = Types.Int } -> true
    | _ -> false
  with Error (error, 0) -> failwith_error error

let%test "recursive_type" =
  try
    let prog =
      {|
 
/* define valid recursive types */
let
/* define a list */
type intlist = {hd: int, tl: intlist} 

/* define a tree */
type tree ={key: int, children: treelist}
type treelist = {hd: tree, tl: treelist}

var lis:intlist := intlist { hd=0, tl= nil } 

in
  lis
end

  |}
    in
    match trans_exp (ast_exp_of_string prog) with
    | { exp = (); ty = Types.Record ([ (sym, Types.Int); _ ], _) }
      when Symbol.equal sym (Symbol.symbol "hd") ->
        true
    | _ -> false
  with Error (error, 0) -> failwith_error error

let%test "recursive procedures" =
  try
    let prog =
      {|
    
/* define valid mutually recursive procedures */
let

function do_nothing1(a: int, b: string)=
		do_nothing2(a+1)

function do_nothing2(d: int) =
		do_nothing1(d, "str")

in
	do_nothing1(0, "str2")
end

|}
    in
    match trans_exp (ast_exp_of_string prog) with
    | { ty = Types.Unit; _ } -> true
    | _ -> false
  with Error (error, _) -> failwith_error error

let%test "recursive functions" =
  try
    let prog =
      {|
    
/* define valid mutually recursive functions */
let

function do_nothing1(a: int, b: string):int=
    (do_nothing2(a+1);0)

function do_nothing2(d: int):string =
    (do_nothing1(d, "str");" ")

in
  do_nothing1(0, "str2")
end

|}
    in
    match trans_exp (ast_exp_of_string prog) with
    | { ty = Types.Int; _ } -> true
    | _ -> false
  with Error (error, _) -> failwith_error error

let%test "correct_if" =
  try
    let prog =
      {|
    
/* correct if */
if (10 > 20) then 30 else 40	
        
|}
    in
    match trans_exp (ast_exp_of_string prog) with
    | { ty = Types.Int; _ } -> true
    | _ -> false
  with Error (error, _) -> failwith_error error

let%test "invalid_if" =
  try
    let prog =
      {|

/* error : types of then - else differ */

if (5>4) then 13 else  " "
        
|}
    in
    let _res = trans_exp (ast_exp_of_string prog) in
    failwith "did not raise error"
  with Error (Expr_type_clash (Types.Int, Types.String), _) -> true

let%test "while_body_not_unit" =
  try
    let prog =
      {|

/* error : body of while not unit */
while(10 > 5) do 5+6
      
|}
    in
    let _res = trans_exp (ast_exp_of_string prog) in
    failwith "did not raise error"
  with Error (Expr_type_clash (Types.Unit, Types.Int), _) -> true

let%test "valid_for_and_let" =
  try
    let prog =
      {|

/* valid for and let */

let
  var a:= 0
in 
  for i:=0 to 100 do (a:=a+1;())
end
      
|}
    in
    match trans_exp (ast_exp_of_string prog) with
    | { ty = Types.Unit; _ } -> true
    | _ -> false
  with Error (error, _) -> failwith_error error