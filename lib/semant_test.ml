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
        sprintf "expected type %s, got type %s" (Types.name expect_ty)
          (Types.name actual_ty)
    | Unbound_type ty -> sprintf "unbound type %s" ty
    | Unbound_value v -> sprintf "unbound value %s" v
    | Unexpected_break -> "unexpected break"
    | Not_a_function s -> sprintf "%s is not a function" s
    | Readonly s -> sprintf "readonly variable %s" s
    | Nonexistent_field s -> sprintf "nonexistent field %s" s
    | Not_an_array -> "not an array"
    | Not_a_record -> "not a record"
    | Not_an_integer -> "not an integer"
    | Not_comparable -> "not comparable"
    | Not_equality -> "not equality"
    | Multiple_bindings s -> sprintf "%s is bound multiple times" s
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

let%test "test5" =
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
  with
  | Error (Expr_type_clash (Types.Int, Types.String), _) -> true
  | Error (error, _) -> failwith_error error

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
  with
  | Error (Expr_type_clash (Types.Name _, Types.Name _), _) -> true
  | Error (error, _) -> failwith_error error

let%test "test17" =
  try
    let prog =
      {|/* error: definition of recursive types is interrupted */
let
/* define a tree */
type tree ={key: int, children: treelist}
var d:int :=0
type treelist = {hd: tree, tl: treelist}

in
  d
end
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Unbound_type "treelist", _) -> true
  | Error (error, _) -> failwith_error error

let%test "test18" =
  try
    let prog =
      {|/* error : definition of recursive functions is interrupted */
let

function do_nothing1(a: int, b: string):int=
    (do_nothing2(a+1);0)

var d:=0

function do_nothing2(d: int):string =
    (do_nothing1(d, "str");" ")

in
  do_nothing1(0, "str2")
end
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Unbound_value "do_nothing2", _) -> true
  | Error (error, _) -> failwith_error error

let%test "test19" =
  try
    let prog =
      {|/* error : second function uses variables local to the first one, undeclared variable */
let

function do_nothing1(a: int, b: string):int=
    (do_nothing2(a+1);0)

function do_nothing2(d: int):string =
    (do_nothing1(a, "str");" ")

in
  do_nothing1(0, "str2")
end
      
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Unbound_value "a", _) -> true
  | Error (error, _) -> failwith_error error

let%test "test20" =
  try
    let prog =
      {|/* error: undeclared variable i */

while 10 > 5 do (i+1;())
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Unbound_value "i", _) -> true
  | Error (error, _) -> failwith_error error

let%test "test21" =
  try
    let prog =
      {|/* error : procedure returns value  and procedure is used in arexpr */
      let
      
      /* calculate n! */
      function nfactor(n: int) =
          if  n = 0 
            then 1
            else n * nfactor(n-1)
      
      in
        nfactor(10)
      end
            
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Expr_type_clash (Types.Int, Types.Unit), _) -> true
  | Error (error, _) -> failwith_error error

let%test "test22" =
  try
    let prog =
      {|/* error : field not in record type */

      let 
        type rectype = {name:string , id:int}
        var rec1 := rectype {name="Name", id=0}
      in
        rec1.nam := "asd"
    end           
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Nonexistent_field "nam", _) -> true
  | Error (error, _) -> failwith_error error

let%test "test23" =
  try
    let prog =
      {|/* error : type mismatch */

let 
  type rectype = {name:string , id:int}
  var rec1 := rectype {name="aname", id=0}
in
  rec1.name := 3;
  rec1.id := "" 
end            
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Expr_type_clash (Types.String, Types.Int), _) -> true
  | Error (error, _) -> failwith_error error

let%test "test24" =
  try
    let prog =
      {|/* error : variable not array */
let 
  var d:=0
in
  d[3]
end
               
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Not_an_array, _) -> true
  | Error (error, _) -> failwith_error error

let%test "test25" =
  try
    let prog =
      {|/* error : variable not record */
let 
  var d:=0
in
  d.f 
end
                       
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Not_a_record, _) -> true
  | Error (error, _) -> failwith_error error

let%test "test26" =
  try
    let prog = {|/* error : integer required */

3 + "var" 
|} in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Expr_type_clash (Types.Int, Types.String), _) -> true
  | Error (error, _) -> failwith_error error

let%test "test28" =
  try
    let prog =
      {|/* error : different record types */

let
  type rectype1 = {name:string , id:int}
  type rectype2 = {name:string , id:int}

  var rec1: rectype1 := rectype2 {name="Name", id=0}
in
  rec1
end    
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Expr_type_clash (Types.Name (syml, _), Types.Name (symr, _)), _)
    when Symbol.equal syml (Symbol.symbol "rectype1")
         && Symbol.equal symr (Symbol.symbol "rectype2") ->
      true
  | Error (error, _) -> failwith_error error

let%test "test29" =
  try
    let prog =
      {|/* error : different array types */

let
  type arrtype1 = array of int
  type arrtype2 = array of int

  var arr1: arrtype1 := arrtype2 [10] of 0
in
  arr1
end      
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Expr_type_clash (Types.Name (syml, _), Types.Name (symr, _)), _)
    when Symbol.equal syml (Symbol.symbol "arrtype1")
         && Symbol.equal symr (Symbol.symbol "arrtype2") ->
      true
  | Error (error, _) -> failwith_error error

let%test "test30" =
  try
    let prog =
      {|/* synonyms are fine */

let 
    type a = array of int
    type b = a

    var arr1:a := b [10] of 0
in
    arr1[2]
end
|}
    in
    match trans_exp_of_string prog with
    | { ty = Types.Int; _ } -> true
    | _ -> false
  with Error (error, _) -> failwith_error error

let%test "test31" =
  try
    let prog =
      {|/* error : type constraint and init value differ */
let 
  var a:int := " "
in
  a
end
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Expr_type_clash (Types.Int, Types.String), _) -> true
  | Error (error, _) -> failwith_error error

let%test "test32" =
  try
    let prog =
      {|/* error : initializing exp and array type differ */

let
  type arrayty = array of int

  var a := arrayty [10] of " "
in
  0
end      
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Expr_type_clash (Types.Int, Types.String), _) -> true
  | Error (error, _) -> failwith_error error

let%test "test33" =
  try
    let prog =
      {|/* error : unknown type */
let
  var a:= rectype {}
in
  0
end
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Unbound_type "rectype", _) -> true
  | Error (error, _) -> failwith_error error

let%test "test34" =
  try
    let prog =
      {|/* error : formals and actuals have different types */
let
  function g (a:int , b:string):int = a
in
  g("one", "two")
end
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Expr_type_clash (Types.Int, Types.String), _) -> true
  | Error (error, _) -> failwith_error error

let%test "test35" =
  try
    let prog =
      {|/* error : formals are more then actuals */
let
  function g (a:int , b:string):int = a
in
  g("one")
end
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Wrong_num_arguments, _) -> true
  | Error (error, _) -> failwith_error error

let%test "test36" =
  try
    let prog =
      {|/* error : formals are fewer then actuals */
let
  function g (a:int , b:string):int = a
in
  g(3,"one",5)
end      
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Wrong_num_arguments, _) -> true
  | Error (error, _) -> failwith_error error

let%test "test37" =
  try
    let prog =
      {|/* redeclaration of variable; this is legal, there are two different
      variables with the same name.  The second one hides the first.  */
let
  var a := 0
  var a := " "
in
  a
end
|}
    in
    match trans_exp_of_string prog with
    | { ty = Types.String; _ } -> true
    | _ -> false
  with Error (error, _) -> failwith_error error

let%test "test38" =
  try
    let prog =
      {|/* This is illegal, since there are two types with the same name
    in the same (consecutive) batch of mutually recursive types. 
    See also test47  */
let
	type a = int
	type a = string
in
	0
end
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Multiple_bindings "a", _) -> true
  | Error (error, _) -> failwith_error error

let%test "test39" =
  try
    let prog =
      {|/* This is illegal, since there are two functions with the same name
    in the same (consecutive) batch of mutually recursive functions.
   See also test48 */
let
	function g(a:int):int = a
	function g(a:int):int = a
in
	0
end
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Multiple_bindings "g", _) -> true
  | Error (error, _) -> failwith_error error

let%test "test40" =
  try
    let prog =
      {|/* error : procedure returns value */
      let
        function g(a:int) = a
      in 
        g(2)
      end
            
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Expr_type_clash (Types.Unit, Types.Int), _) -> true
  | Error (error, _) -> failwith_error error

let%test "test41" =
  try
    let prog =
      {|/* local types hide global */
let
  type a = int
in
  let
    type a = string
    var x:a := "hello"
  in
    x
  end
end      
|}
    in
    match trans_exp_of_string prog with
    | { ty = Types.Name (_, { contents = Some Types.String }); _ } -> true
    | _ -> false
  with Error (error, _) -> failwith_error error

let%test "test42" =
  try
    let prog =
      {|/* correct declarations */
let 

type arrtype1 = array of int
type rectype1 = {name:string, address:string, id: int , age: int}
type arrtype2 = array of rectype1
type rectype2 = {name : string, dates: arrtype1}

type arrtype3 = array of string

var arr1 := arrtype1 [10] of 0
var arr2  := arrtype2 [5] of rectype1 {name="aname", address="somewhere", id=0, age=0}
var arr3:arrtype3 := arrtype3 [100] of ""

var rec1 := rectype1 {name="Kapoios", address="Kapou", id=02432, age=44}
var rec2 := rectype2 {name="Allos", dates= arrtype1 [3] of 1900}

in

arr1[0] := 1; 
arr1[9] := 3;
arr2[3].name := "kati";
arr2[1].age := 23;
arr3[34] := "sfd";

rec1.name := "sdf";
rec2.dates[0] := 2323;
rec2.dates[2] := 2323

end
|}
    in
    match trans_exp_of_string prog with
    | { ty = Types.Unit; _ } -> true
    | _ -> false
  with Error (error, _) -> failwith_error error

let%test "test43" =
  try
    let prog =
      {|/* initialize with unit and causing type mismatch in addition */

let 
  var a := ()
in
  a + 3
end
|}
    in
    let _res = trans_exp_of_string prog in
    failwith "did not raise error"
  with
  | Error (Not_an_integer, _) -> true
  | Error (error, _) -> failwith_error error

let%test "test44" =
  try
    let prog =
      {|/* valid nil initialization and assignment */
let 

  type rectype = {name:string, id:int}
  var b:rectype := nil

in

  b := nil

end      
|}
    in
    match trans_exp_of_string prog with
    | { ty = Types.Unit; _ } -> true
    | _ -> false
  with Error (error, _) -> failwith_error error

let%test "test45" =
  try
    let prog =
      {|/* valid nil initialization and assignment */
let 

  type rectype = {name:string, id:int}
  var b:rectype := nil

in

  b := nil

end      
|}
    in
    match trans_exp_of_string prog with
    | { ty = Types.Unit; _ } -> true
    | _ -> false
  with Error (error, _) -> failwith_error error

(* DO A SIMILAR CASE FOR RECORDS, ALSO MAKE SURE CAN SUBSCRIPT W/ NAMED INT TYPES *)
