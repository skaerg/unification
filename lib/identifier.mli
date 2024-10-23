type t = Id of string
[@@deriving eq, ord, show]

val fresh : unit -> t
