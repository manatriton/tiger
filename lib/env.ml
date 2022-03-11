open Core

type ty = Types.ty

type entry =
  | VarEntry of { ty : ty }
  | FunEntry of { formals : ty list; result : ty }

let base_tenv =
  let predefined = [ ("int", Types.Int); ("string", Types.String) ] in
  List.fold predefined ~init:Symbol.empty ~f:(fun acc def ->
      let name, ty = def in
      Symbol.add acc ~symbol:(Symbol.symbol name) ~data:ty)

let base_venv =
  let predefined =
    [
      ("print", [ Types.String ], Types.Unit);
      ("flush", [], Types.Unit);
      ("getchar", [], Types.String);
      ("ord", [ Types.String ], Types.Int);
      ("chr", [ Types.Int ], Types.String);
      ("size", [ Types.String ], Types.Int);
      ("substring", [ Types.String; Types.Int; Types.Int ], Types.String);
      ("concat", [ Types.String; Types.String ], Types.String);
      ("not", [ Types.Int ], Types.Int);
      ("exit", [ Types.Int ], Types.Unit);
    ]
  in
  List.fold predefined ~init:Symbol.empty ~f:(fun acc def ->
      let name, formals, result = def in
      Symbol.add acc ~symbol:(Symbol.symbol name)
        ~data:(FunEntry { formals; result }))
