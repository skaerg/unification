(* keys = Identifier ; values = Type.t *)
module IdMap = Map.Make(Identifier)
type t = Type.t IdMap.t

(* pretty print *)


let pp (ppf : Stdlib.Format.formatter) (id_ty_map : t) = 
  
  Format.fprintf ppf "\n"; 
  IdMap.iter 
    (fun (Identifier.Id(id)) ty -> (
      Format.fprintf ppf "( key = %s \n value = %a ) ;\n\n" id Type.pp ty
    )) 
    id_ty_map


(* equality *)
let equal (t1 : t) (t2 : t) =
  IdMap.equal Type.equal t1 t2


(** Applies a syntactic substitution to a type *)
let rec apply (s : t) (ty : Type.t) = match ty with
  | Var id -> 
    let ty_opt = IdMap.find_opt id s in
    (match ty_opt with
    | Some new_ty -> new_ty
    | None        -> ty)
  | Int -> Type.Int
  | Product (ty1,ty2) -> Type.Product ((apply s ty1),(apply s ty2))
  | Arrow (ty1,ty2) -> Type.Arrow ((apply s ty1),(apply s ty2))


(** Computes the substitution obtained by composing two given substitutions (first s1, then s2, i.e. s2 ◦ s1). *)
let compose (s2 : t) (s1 : t) =

  IdMap.merge (fun _ ty1 ty2 -> 
    match (ty1,ty2) with

    (* If there is a binding (id,ty) in s1, then the value associated
       to the key id in the composition (s2 * s1) is equal to [apply s2 ty]. *)
    | ((Some ty),_)   -> Some (apply s2 ty)

    (* Otherwise, if there is a binding (id,ty) in s2, then the value ty
       will be bound to the key id in the composition (s2 * s1) *)
    | None,(Some ty)  -> Some ty

    (* Otherwise, no binding is created *)
    | None,None       -> None
    
    ) (s1) (s2)
  

let bindings_to_subst ( bindings : (Identifier.t * Type.t) list ) =
  List.fold_left 
    (fun sub (id,ty) -> IdMap.add id ty sub)
    (IdMap.empty)
    (bindings)

