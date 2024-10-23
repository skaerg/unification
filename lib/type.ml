type t =
  | Var of Identifier.t
  | Int
  | Product of t * t
  | Arrow of t * t
  [@@deriving eq, ord, show]


module IdToString = struct

  (* keys = Identifier.t ; values = string *)
  module IdMap = Map.Make(Identifier)
  type t = string IdMap.t

  let empty = IdMap.empty

  let next_ty (ls : char list) =
  
    let rec aux (ls : char list) (acc : char list)=
      match ls with
      | [] -> List.rev acc
      | x::tl ->
        let x_int = int_of_char x in
        if (x_int = 122) then 
          if (tl = []) then (aux tl ('a'::['a']@acc))
          else (aux tl ('a'::acc))
        else ( List.rev ((char_of_int (x_int+1))::acc)) @ tl
    in

    aux ls []

  
  let str_of_ls (ls : char list) =
    "'" ^ (List.fold_left (fun acc e -> String.make 1 e ^ acc) "" ls)


  let get_ty_var (id : Identifier.t) (ty_dict : t) (current : char list) =
    match (IdMap.find_opt id ty_dict) with
    | Some ty_var -> ty_var,ty_dict,current
    | None        -> 
      let ty_var = str_of_ls current in
      let ty_dict = IdMap.add id ty_var ty_dict in
      let next_ty = next_ty current in
      (ty_var,ty_dict,next_ty)


end



module IdMap = Map.Make(Identifier)

let alpha_eq (t1 : t) (t2 : t) =
  
  let rec aux t1 t2 map =
    match (t1,t2) with
    | Var id1,Var id2 -> 

      (match (IdMap.find_opt id1 map) with
      (* if the key id1 is already bound to a value id3 in the map, 
         then t1 and t2 are alpha equivalent if and only if id2 is equal to id3 *)
      | Some id3  -> 
        if (Identifier.equal id2 id3) then true,map else false,map

      (* otherwise, there is no binding for id1 in the map, in which case
         the binding (id1,id2) is added to the map, and the types t1 and t2 
         are alpha equivalent if and only if no value is equal to t2 in the existing
         key value pairs in the map. *)
      | None      -> 
        (IdMap.for_all (fun _ v -> (not (Identifier.equal id2 v))) map),
        (IdMap.add id1 id2 map))
      
    | Int,Int -> true,map
    | Arrow(t1a,t1b),Arrow(t2a,t2b)
    | Product(t1a,t1b),Product(t2a,t2b) ->
      let left,map2 = (aux t1a t2a map) in
      if left then (aux t1b t2b map2) else false,map
    | _,_ -> false,map
  in

  fst (aux t1 t2 IdMap.empty)



let rec is_free (ty : t) (id : Identifier.t) =

  match ty with
  | Var id2 -> Identifier.equal id id2
  | Int -> false
  | Product(t1,t2) | Arrow(t1,t2) ->
    (is_free t1 id) || (is_free t2 id)



let to_string (ty : t) =

  let rec aux (ty : t) (ty_dict : IdToString.t) (current : char list) =
    match ty with
    | Var id -> IdToString.get_ty_var id ty_dict current
    | Int -> "int",ty_dict,current
    | Product(ty1,ty2) -> 
      
      (match ty1,ty2 with
      | Arrow(_,_),Arrow(_,_) 
      | Product(_,_),Product(_,_) 
      | Product(_,_),Arrow(_,_)
      | Arrow(_,_),Product(_,_) ->
        let ty_var1,ty_dict,next = (aux ty1 ty_dict current) in
        let ty_var2,ty_dict,next2 = (aux ty2 ty_dict next) in
        (("(" ^ ty_var1 ^ ") * (" ^ ty_var2 ^ ")"),ty_dict,next2)
      | Arrow(_,_),_ 
      | Product(_,_),_ -> 
        let ty_var1,ty_dict,next = (aux ty1 ty_dict current) in
        let ty_var2,ty_dict,next2 = (aux ty2 ty_dict next) in
        ( "(" ^ ty_var1 ^ ") * " ^ ty_var2),ty_dict,next2
      | _,Arrow(_,_) 
      | _,Product(_,_) -> 
        let ty_var1,ty_dict,next = (aux ty1 ty_dict current) in
        let ty_var2,ty_dict,next2 = (aux ty2 ty_dict next) in
        (ty_var1 ^ " * (" ^ ty_var2 ^ ")"),ty_dict,next2
      | _,_ -> 
        let ty_var1,ty_dict,next = (aux ty1 ty_dict current) in
        let ty_var2,ty_dict,next2 = (aux ty2 ty_dict next) in
        (ty_var1 ^ " * " ^ ty_var2),ty_dict,next2 )

    | Arrow(ty1,ty2) -> 
      (match ty1,ty2 with
      | Arrow(_,_),_          -> 
        let ty_var1,ty_dict,next = (aux ty1 ty_dict current) in
        let ty_var2,ty_dict,next2 = (aux ty2 ty_dict next) in
        ("(" ^ ty_var1 ^ ") -> " ^ ty_var2),ty_dict,next2
      | _,_                   -> 
        let ty_var1,ty_dict,next = (aux ty1 ty_dict current) in
        let ty_var2,ty_dict,next2 = (aux ty2 ty_dict next) in
        ((ty_var1 ^ " -> " ^ ty_var2) ),ty_dict,next2 )

  in

  let (res,_,_) = (aux ty IdToString.empty ['a']) in res