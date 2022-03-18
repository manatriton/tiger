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

let rec equal tyl tyr =
  match (tyl, tyr) with
  | Record (_, lunique), Record (_, runique) -> phys_equal lunique runique
  | Nil, Nil -> true
  | Int, Int -> true
  | String, String -> true
  | Array (_, lunique), Array (_, runique) -> phys_equal lunique runique
  | Name (_, { contents = Some tyl }), tyr -> equal tyl tyr
  | tyl, Name (_, { contents = Some tyr }) -> equal tyl tyr
  | Unit, Unit -> true
  | _ -> false

let rec is_assignable ~dst ~src =
  match (dst, src) with
  | Name (_, { contents = Some dst }), Name (_, { contents = Some src }) ->
      is_assignable ~dst ~src
  | Name (_, { contents = Some dst }), src -> is_assignable ~dst ~src
  | dst, Name (_, { contents = Some src }) -> is_assignable ~dst ~src
  | Record _, Nil -> true
  | dst, src -> equal dst src

let name ty =
  match ty with
  | Record _ -> "record"
  | Nil -> "nil"
  | Int -> "int"
  | String -> "string"
  | Array _ -> "array"
  | Name (sym, _) -> Symbol.name sym
  | Unit -> "unit"

let rec can_test_equality ty =
  match ty with
  | Name (_, { contents = Some ty' }) -> can_test_equality ty'
  | Unit | Nil -> false
  | _ -> true

let rec comparable ty =
  match ty with
  | String | Int -> true
  | Name (_, { contents = Some ty' }) -> comparable ty'
  | _ -> false

let rec is_int ty =
  match ty with
  | Int -> true
  | Name (_, { contents = Some ty' }) -> is_int ty'
  | _ -> false