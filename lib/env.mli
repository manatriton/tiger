(* type access *)
type ty = Types.ty

type entry =
  | VarEntry of { ty : ty }
  | FunEntry of { formals : ty list; result : ty }

val base_tenv : ty Symbol.table
val base_venv : entry Symbol.table