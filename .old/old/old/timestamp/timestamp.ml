(* timestamps *)

type t

val get_timestamp: unit -> t
val diff: t -> t -> t;
val to_string: t -> string

