type venv = Env.entry Symbol.table
type tenv = Env.ty Symbol.table
type expty = { exp: Translate.exp; ty: Env.ty }

val trans_var : venv -> tenv -> Ast.var -> expty
val trans_exp : venv -> tenv -> Ast.exp -> expty
val trans_dec : venv -> tenv -> Ast.dec -> venv * tenv
val trans_ty : venv -> tenv -> Ast.ty -> Env.ty
