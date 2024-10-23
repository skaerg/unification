type binop =
  | Plus | Minus | Times | Div
  [@@deriving eq, ord, show]

type projection = First | Second
  [@@deriving eq, ord, show]

type t =
  | Var of Identifier.t
  | IntConst of int
  | Binop of t * binop * t
  | Pair of t * t
  | Proj of projection * t
  | Fun of Identifier.t * t
  | App of t * t
  [@@deriving eq, ord, show]


let to_string (term : t) =

  let rec aux term =
    match term with
    | Var (Id id) -> id
    | IntConst n -> string_of_int n
    | Binop (t1,binop,t2) -> 
      let binop_str = (
        match binop with
        | Plus -> " + "
        | Minus -> " - "
        | Times -> " * "
        | Div -> " / ") in
      ("(" ^ (aux t1) ^ binop_str ^ (aux t2) ^ ")")
    | Pair (t1,t2) ->
      ("(" ^ (aux t1) ^ " , " ^ (aux t2) ^ ")")
    | Proj (proj,t2) ->
      let proj_str = (
        match proj with
        | First -> "fst "
        | Second -> "snd " ) in
      (proj_str ^ "(" ^ (aux t2) ^ ")")
    | Fun ((Id id),t2) ->
      ("(fun "^ id ^ " -> " ^ (aux t2) ^ ")")
    | App (t1,t2) ->
      ( "(" ^ (aux t1) ^ " " ^ (aux t2) ^ ")")
  in

  match term with
  | Var (Id id) -> id
  | IntConst n -> string_of_int n
  | Binop (t1,binop,t2) -> 
    let binop_str = (
      match binop with
        | Plus -> " + "
        | Minus -> " - "
        | Times -> " * "
        | Div -> " / ") in
    ("(" ^ (aux t1) ^ binop_str ^ (aux t2) ^ ")")
  | Pair (t1,t2) ->
    ("(" ^ (aux t1) ^ " , " ^ (aux t2) ^ ")")
  | Proj (proj,t2) ->
    let proj_str = (
      match proj with
      | First -> "fst "
      | Second -> "snd " ) in
    (proj_str ^ (aux t2))
  | Fun ((Id id),t2) ->
    ("fun "^ id ^ " -> " ^ (aux t2))
  | App (t1,t2) ->
    ( (aux t1) ^ " " ^ (aux t2))