type t [@@deriving sexp]

val symbol : string -> t 
val name : t -> string 

type 'a table

val empty : 'a table 
val add: 'a table -> symbol:t-> data:'a -> 'a table
val find: 'a table -> symbol:t -> 'a option
