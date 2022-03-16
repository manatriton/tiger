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
 
let
  type intlist = {hd: int, tl: intlist}
  var lis: intlist := intlist { hd = 0, tl = nil }
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
