open Core

type t = string * int [@@deriving sexp]

let next_symbol = ref 0
let hashtbl = Hashtbl.create ~size:17 (module String)

let symbol name =
  match Hashtbl.find hashtbl name with
  | Some i -> (name, i)
  | None ->
      let i = !next_symbol in
      next_symbol := i + 1;
      Hashtbl.set hashtbl ~key:name ~data:i;
      (name, i)

let name (s, _i) = s

type 'a table = (int, 'a, Int.comparator_witness) Map.t

let empty = Map.empty (module Int)
let add t ~symbol:(_, key) ~data = Map.set t ~key ~data
let find t ~symbol:(_, key) = Map.find t key
