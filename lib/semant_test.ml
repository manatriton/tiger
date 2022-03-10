open Ast
open! Core

let venv = Env.base_venv
let tenv = Env.base_tenv
let trans_exp = Semant.trans_exp venv tenv

let%test "nil_exp" =
  match trans_exp NilExp with
  | { exp = (); ty = Types.Nil } -> true
  | _ -> false

let%test "int_exp" =
  match trans_exp (IntExp 3) with
  | { exp = (); ty = Types.Int } -> true
  | _ -> false

let%test "string_exp" =
  match trans_exp (StringExp ("hello", 0)) with
  | { exp = (); ty = Types.String } -> true
  | _ -> false

let%test "seq_exp_empty" =
  let exps = [] in
  match trans_exp (SeqExp exps) with
  | { exp = (); ty = Types.Unit } -> true
  | _ -> false

let%test "seq_exp_multi" =
  let exps =
    [
      (IntExp 3, 0);
      (StringExp ("hello", 0), 0);
      (NilExp, 0);
      (StringExp ("hello", 0), 0);
    ]
  in
  match trans_exp (SeqExp exps) with
  | { exp = (); ty = Types.String } -> true
  | _ -> false

let%test "assign_exp" =
  let var = SimpleVar (Symbol.symbol "x", 0) in
  let exp = IntExp 0 in
  match Semant.trans_exp venv tenv (AssignExp { var; exp; pos = 0 }) with
  | { exp = (); ty = Types.Unit } -> true
  | _ -> false

(* let%test "if_exp_no_else" =
   let test = IntExp 0 in
   let then' = Ass in
   match trans_exp (IfExp { test; then'; else' = None; pos = 0 }) with
   | { exp = (); ty = Types.Unit } -> true
   | _ -> false *)

let%test "if_exp_else" =
  let test = IntExp 0 in
  let then' = IntExp 1 in
  let else' = Some (IntExp 2) in
  match trans_exp (IfExp { test; then'; else'; pos = 0 }) with
  | { exp = (); ty = Types.Int } -> true
  | _ -> false
