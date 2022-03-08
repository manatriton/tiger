open Core

type symbol = string * int [@@deriving sexp]

let next_symbol = ref 0
let hashtbl = Hashtbl.create (module String)

let symbol name =
  match Hashtbl.find hashtbl name with
  | Some i -> (name, i)
  | None ->
      let i = !next_symbol in
      next_symbol := i + 1;
      (name, i)

let name (s, _i) = s
