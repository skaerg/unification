module IdMap : Map.S with type key = Identifier.t
type t = Type.t IdMap.t

(* pretty print *)
val pp : Stdlib.Format.formatter -> t -> unit

(* equality *)
val equal : t -> t -> bool

(* apply a syntactic substitution to a given type *)
val apply : t -> Type.t -> Type.t

(* compose s2 s1 : first s1, then s2 (i.e. s2 ◦ s1) *)
val compose : t -> t -> t

(* returns a substitution that contains the given list of bindings *)
val bindings_to_subst : (Identifier.t * Type.t) list -> t