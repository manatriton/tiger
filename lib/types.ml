open Core

type unique = unit ref [@@deriving sexp]

type ty =
  | Record of (Symbol.t * ty) list * unique
  | Nil
  | Int
  | String
  | Array of ty * unique
  | Name of Symbol.t * ty option ref
  | Unit
[@@deriving sexp]

let rec equal x y =
  match (x, y) with
  | Record _, Record _ -> true
  | _ty, Nil -> true
  | Int, Int -> true
  | String, String -> true
  | Array _, Array _ -> true
  | Name (_, { contents = Some tyl }), tyr -> equal tyl tyr
  | Unit, Unit -> true
  | _ -> false