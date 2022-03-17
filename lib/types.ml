open Core

type unique = unit ref [@@deriving sexp]

exception Error

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
  | Record (_, lunique), Record (_, runique) -> phys_equal lunique runique
  | _ty, Nil -> true
  | Int, Int -> true
  | String, String -> true
  | Array (_, lunique), Array (_, runique) -> phys_equal lunique runique
  | Name (_, { contents = Some tyl }), tyr -> equal tyl tyr
  | tyl, Name (_, { contents = Some tyr }) -> equal tyl tyr
  | Unit, Unit -> true
  | _ -> false
