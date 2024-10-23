(*
   This module contains the syntax of terms of a minimal programming language.
*)
type binop =
  | Plus | Minus | Times | Div
  [@@deriving eq, ord, show]

type projection = First | Second
  [@@deriving eq, ord, show]

(*
   Every value of type Term.t is the abstract syntax tree of a program.
*)
type t =
  | Var of Identifier.t
  | IntConst of int
  | Binop of t * binop * t
  | Pair of t * t
  | Proj of projection * t
  | Fun of Identifier.t * t
  | App of t * t
  [@@deriving eq, ord, show]


val to_string : t -> string