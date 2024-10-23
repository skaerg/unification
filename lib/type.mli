type t =
  | Var of Identifier.t
  | Int
  | Product of t * t
  | Arrow of t * t
  [@@deriving eq, ord, show]

(** Returns true if a given identifier is free in a given type, 
    and false otherwise. *)
val is_free : t -> Identifier.t -> bool

(** `alpha_eq t1 t2` returns true if t1 and t2 are alpha-equivalent, 
    i.e. their only difference is the renaming of the identifiers of Var constructors. *)
val alpha_eq : t -> t -> bool

(** Returns a string that represents the OCaml-style type
    associated with the given type.
    Two alpha-equivalent types will yield the same output. *)
val to_string : t -> string