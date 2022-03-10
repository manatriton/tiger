type unique = unit ref

type ty =
  | Record of (Symbol.t * ty) list * unique
  | Nil
  | Int
  | String
  | Array of ty * unique
  | Name of Symbol.t * ty option ref
  | Unit

let equal x y =
  match (x, y) with
  | Record _, Record _ -> true
  | Nil, Nil -> true
  | Int, Int -> true
  | String, String -> true
  | Array _, Array _ -> true
  | Name _, Name _ -> true
  | Unit, Unit -> true
  | _ -> false