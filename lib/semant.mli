type venv = Env.entry Symbol.table
type tenv = Env.ty Symbol.table
type expty = { exp: Translate.exp; ty: Env.ty }
type error =
  | Wrong_num_arguments
  | Expr_type_clash of Types.ty * Types.ty 
  (* | Duplicate_record_field *)
  | Unbound_value of string
  | Unbound_type of string
  | Unexpected_break

exception Error of error * int

val trans_var : venv -> tenv -> Ast.var -> expty
val trans_exp : venv -> tenv -> Ast.exp -> expty
val trans_dec : venv -> tenv -> Ast.dec -> venv * tenv
val trans_ty : venv -> tenv -> Ast.ty -> Env.ty
