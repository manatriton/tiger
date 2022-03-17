open! Core
open Semant

let venv = Env.base_venv
let tenv = Env.base_tenv
let trans_exp = Semant.trans_exp venv tenv

let ast_exp_of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let trans_exp_of_string prog = trans_exp (ast_exp_of_string prog)

let failwith_error error =
  let s =
    match error with
    | Wrong_num_arguments -> "wrong number of arguments"
    | Expr_type_clash (expect_ty, actual_ty) ->
        sprintf "expected type %s, got type %s"
          (Sexp.to_string (Types.sexp_of_ty expect_ty))
          (Sexp.to_string (Types.sexp_of_ty actual_ty))
    | Unbound_type ty -> sprintf "unbound type %s" ty
    | Unbound_value v -> sprintf "unbound value %s" v
    | Unexpected_break -> "unexpected break"
    | Not_a_function _s -> "not a function"
    | Readonly s -> sprintf "readonly variable %s" s
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

let%test "test1" =
  try
    let prog =
      {|
  
/* an array type and an array variable */
let
	type  arrtype = array of int
	var arr1:arrtype := arrtype [10] of 0
in
	arr1
end

  |}
    in
    match trans_exp_of_string prog with
    | {
     ty = Types.Name (_, { contents = Some (Types.Array (Types.Int, _)) });
     _;
    } ->
        true
    | _ -> false
  with Error (error, _) -> failwith_error error

let%test "test2" =
  try
    let prog =
      {|
  
/* arr1 is valid since expression 0 is int = myint */
let
  type myint = int
  type  arrtype = array of myint

  var arr1:arrtype := arrtype [10] of 0
in
  arr1
end

  |}
    in
    match trans_exp_of_string prog with
    | {
     ty =
       Types.Name
         ( _,
           {
             contents =
               Some
                 (Types.Array
                   (Types.Name (sym, { contents = Some Types.Int }), _));
           } );
     _;
    }
      when Symbol.equal sym (Symbol.symbol "myint") ->
        true
    | { ty; _ } -> failwith (Sexp.to_string (Types.sexp_of_ty ty))
  with Error (error, _) -> failwith_error error

let%test "test3" =
  try
    let prog =
      {|
  
/* a record type and a record variable */
let
  type  rectype = {name:string, age:int}
  var rec1:rectype := rectype {name="Nobody", age=1000}
in
  rec1.name := "Somebody";
  rec1
end

  |}
    in
    match trans_exp_of_string prog with
    | { ty = Types.Name (sym, { contents = Some (Types.Record _) }); _ }
      when Symbol.equal sym (Symbol.symbol "rectype") ->
        true
    | _ -> false
  with Error (error, _) -> failwith_error error

let%test "test4" =
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

let%test "test_5" =
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
    match trans_exp_of_string prog with
    | {
     exp = ();
     ty =
       Types.Name
         (_, { contents = Some (Types.Record ([ (sym, Types.Int); _ ], _)) });
    }
      when Symbol.equal sym (Symbol.symbol "hd") ->
        true
    | _ -> false
  with Error (error, 0) -> failwith_error error

let%test "test6" =
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

let%test "test7" =
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

let%test "test8" =
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

let%test "test9" =
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

let%test "test10" =
  try
    let prog =
      {|

/* error : body of while not unit */
while(10 > 5) do 5+6
      
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with Error (Expr_type_clash (Types.Unit, Types.Int), _) -> true

let%test "test11" =
  try
    let prog =
      {|

/* error hi expr is not int, and index variable erroneously assigned to.  */
for i:=10 to " " do 
  i := i - 1
      
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Expr_type_clash (Types.Int, Types.String), _) -> true
  | Error (error, 0) -> failwith_error error

let%test "test11.1" =
  try
    let prog =
      {|

/* error hi expr is not int, and index variable erroneously assigned to.  */
for i:=10 to 11 do 
  i := i - 1
      
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Readonly _, _) -> true
  | Error (error, _) -> failwith_error error

let%test "test12" =
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
    match trans_exp_of_string prog with
    | { ty = Types.Unit; _ } -> true
    | _ -> false
  with Error (error, _) -> failwith_error error

let%test "test13" =
  try
    let prog =
      {|

/* error: comparison of incompatible types */

3 > "df"
      
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with Error (error, _) -> failwith_error error

let%test "test14" =
  try
    let prog =
      {|

/* error : compare rec with array */

let

  type arrtype = array of int
  type rectype = {name:string, id: int}

  var rec := rectype {name="aname", id=0}
  var arr := arrtype [3] of 0

in
  if rec <> arr then 3 else 4
end
      
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with _ -> true