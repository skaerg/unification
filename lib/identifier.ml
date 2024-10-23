type t = Id of string
[@@deriving eq, ord, show]

let fresh_id = ref 0

(** Returns a fresh identifier (i.e. one that has not been used previously). *)
let fresh () =
  fresh_id := !fresh_id + 1;
  Id ("id" ^ (string_of_int (!fresh_id)))