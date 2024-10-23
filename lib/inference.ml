
(* keys = Term.t ; values = Identifier.t *)
module TermMap = Map.Make(Term)
type t = Identifier.t TermMap.t


module IdMap = TypeSubstitution.IdMap


module Equation = struct
  type t = Type.t * Type.t
  let compare ((l1,r1) : t) ((l2,r2) : t) = 
    match (Type.compare l1 l2) with
    | 0 -> (Type.compare r1 r2)
    | n -> n

  (*
  let pp (ppf : Stdlib.Format.formatter) ((l,r) : t) = 
    Format.fprintf ppf "%a = %a" Type.pp l Type.pp r
  *)
end

module EqSet = Set.Make(Equation)

(*
let pp (ppf : Stdlib.Format.formatter) (eqset : EqSet.t) = 

  EqSet.iter (fun eq -> 
    Equation.pp ppf eq; 
    Format.fprintf ppf "\n") eqset

*)



(** Returns a TermMap that maps each identifier & each expression of a term t to a corresponding type variable *)
let get_type_variables (term : Term.t) =

  (* Create a binding for a given term in the map ty_vars 
     if and only if one does not already exist *)
  let update_first (term : Term.t) (ty_vars : t) =
    TermMap.update
    term
    (fun term2_opt ->
      (match term2_opt with
      | Some term2  -> Some term2 
      | None        -> Some ( Identifier.fresh () )))
    ty_vars
  in
  
  let rec aux (term : Term.t) (ty_vars : t) =
    match term with 
    | Var _ -> update_first term ty_vars
    | IntConst _ -> update_first term ty_vars
    | Binop (t1,binop,t2) -> 
      let ty_vars = update_first (Binop (t1,binop,t2)) ty_vars in
      union_first t1 t2 ty_vars
    | Pair (t1,t2) -> 
      let ty_vars = update_first (Pair (t1,t2)) ty_vars in
      union_first t1 t2 ty_vars
    | Proj (proj,t') -> 
      let ty_vars = update_first (Proj (proj,t')) ty_vars in
      aux t' ty_vars
    | Fun (id,t') -> 
      let ty_vars = update_first (Fun (id,t')) ty_vars in
      union_first (Var id) t' ty_vars
    | App (t1,t2) -> 
      let ty_vars = update_first (App (t1,t2)) ty_vars in
      union_first t1 t2 ty_vars
  
  (* Returns the union of (aux t1 ty_vars) and (aux t2 ty_vars).
     If a given binding appears in both maps, 
     keep the corresponding value from (aux t1 ty_vars). *)
  and union_first t1 t2 ty_vars =
    TermMap.union 
      (fun _ ty_var_id _ -> Some ty_var_id )
      (aux t1 ty_vars)
      (aux t2 ty_vars)
  in
  
  aux term (TermMap.singleton term (Identifier.fresh ()))



(* Get the type variable that corresponds to a given term among
   the map of type variables ty_vars *)
let ty_var_of_term (ty_vars : t) (term : Term.t) =
  match (TermMap.find_opt term ty_vars) with 
    | Some key  -> Type.Var key
    | None      -> raise Not_found 



(** Returns a set of equations corresponding to a given term and set of type variables
    that does not contain any trivial equations. *)
let compute_system (ty_vars : t) (term : Term.t) =

  (* Return a set of equations for a given term and each of its sub-terms *)
  let rec aux (term : Term.t) (equations : EqSet.t) =
    
    (* type variable that corresponds to the current term *)
    let ty_var = ty_var_of_term ty_vars term in
    
    (* add an equation for the current term and each of its sub-terms *)
    match term with 
    | Var _ -> EqSet.add (ty_var,ty_var) equations
    

    | IntConst _ -> 
      EqSet.add (ty_var,Type.Int) equations
    

    | Binop (t1,_,t2) -> 

      let ty_var1 = ty_var_of_term ty_vars t1 in
      let ty_var2 = ty_var_of_term ty_vars t2 in

      (* ty_var = Int *)
      (* ty_var1 = Int *)
      (* ty_var2 = Int *)
      let equations = EqSet.add (ty_var2,Type.Int) 
        (EqSet.add (ty_var1,Type.Int) 
        (EqSet.add (ty_var,Type.Int) equations)) in

      (* compute equations for terms t1 and t2 *)
      aux t2 (aux t1 equations)
    
    
    | Pair (t1,t2) -> 

      let ty_var1 = ty_var_of_term ty_vars t1 in
      let ty_var2 = ty_var_of_term ty_vars t2 in

      (* ty_var = ty_var1 * ty_var2 *)
      let eq = EqSet.add (ty_var,Type.(Product (ty_var1,ty_var2))) equations in

      (* compute equations for terms t1 and t2 *)
      aux t2 (aux t1 eq)

    
    | Proj (proj,t') -> 

      let ty_var' = ty_var_of_term ty_vars t' in

      let ty_var1 = Type.Var (Identifier.fresh ()) in
      let ty_var2 = Type.Var (Identifier.fresh ()) in

      let eq = EqSet.add 
        (ty_var,
        (match proj with
        (* ty_var = ty_var1 *)
        | First   -> ty_var1
        (* ty_var = ty_var2 *)
        | Second  -> ty_var2))
        equations
      in

      (* ty_var' = ty_var1 * ty_var2 *)
      let eq = EqSet.add (ty_var',Type.(Product (ty_var1,ty_var2))) eq in

      (* compute equations for the term t' *)
      aux t' eq

    
    | Fun (id,t') -> 
      let ty_var_id = ty_var_of_term ty_vars (Term.Var id) in
      let ty_var' = ty_var_of_term ty_vars t' in

      (* ty_var = ty_var_id -> ty_var' *)
      let eq = EqSet.add (ty_var, Type.(Arrow (ty_var_id,ty_var'))) equations in
      aux t' (aux (Term.Var id) eq)

    
    | App (t1,t2) -> 
      let ty_var1 = ty_var_of_term ty_vars t1 in
      let ty_var2 = ty_var_of_term ty_vars t2 in

      (* ty_var1 = ty_var2 -> ty_var *)
      let eq = EqSet.add (ty_var1, Type.(Arrow (ty_var2,ty_var))) equations in

      aux t2 (aux t1 eq)
  in
  
  (aux term (EqSet.empty))



let eliminate_var (system : EqSet.t) (id : Identifier.t) (ty : Type.t) =

  let sub = TypeSubstitution.bindings_to_subst [ (id,ty) ] in

  EqSet.map
    (fun (l,r) -> (TypeSubstitution.apply sub l),(TypeSubstitution.apply sub r))
    system



let solve_system (system : EqSet.t) =

  let rec aux system sub =

    (* TRIVIAL EQUATION *)
    let trivial_eq_rule system sub = 
      let trivial_opt = EqSet.find_first_opt (fun (l,r) -> l=r) system in

      (match trivial_opt with
      (* Finds the first trivial equation and removes it from the system *)
      | Some eq -> aux (EqSet.remove eq system) sub
      
      (* If there is no such equation, return the associated substitution *)
      | None    -> Some sub )
    in


    (* VARIABLE ORIENTATION *)
    let var_orientation_rule system sub =
      let var_orient_opt = EqSet.find_first_opt
        (fun (l,r) -> 
          match (l,r) with
          | Var(_),Var(_) -> false
          | _,Var(_)      -> true 
          | _,_           -> false ) system in

      (match var_orient_opt with
      | Some ((l,Var(id)) as eq) ->

        (* remove the current equation *)
        let system = EqSet.remove eq system in 

        (* add the same equation with the right and left sides swapped *)
        let system = EqSet.add (Var(id),l) system in

        (* unification *)
        (match (Unification.unify l (Var id)) with 
        | Some sub2 -> aux system (TypeSubstitution.compose sub2 sub)
        | None      -> None)

      (* otherwise, try to apply the trivial equation rule *)
      | _ -> trivial_eq_rule system sub)
    in


    (* VARIABLE ELIMINATION *)
    let var_elimination_rule system sub =
      let elimination_opt = EqSet.find_first_opt
        (fun (l,r) -> 
          match (l,r) with 
          | Var(id),r -> not (Type.is_free r id)
          | _         -> false ) system in

      (match elimination_opt with 
      (* If such an equation is found, replace id by r everywhere else in the system *)
      | Some ((Var(id),r) as eq) -> 
        
        (* Delete equation from the system *)
        let system = EqSet.remove eq system in

        (* Replace id by r everywhere else in the system *)
        let system = eliminate_var system id r in

        (* unification *)
        (match (Unification.unify (Var id) r) with 
        | Some sub2 -> aux system (TypeSubstitution.compose sub2 sub)
        | None      -> None)
      
      (* otherwise, try to apply the variable orientation rule *)
      | _ -> var_orientation_rule system sub)
    in


    (* OCCUR CHECK *)
    let occur_check_rule system sub = 
      let occur_opt = EqSet.find_first_opt
        (fun (l,r) -> 
          match (l,r) with 
          | Var _, Var _  -> false
          | Var(id),r     -> Type.is_free r id
          | _             -> false ) system in

      (match occur_opt with 
      (* If such an equation is found, the system is unsolvable *)
      | Some _ -> None
      
      (* otherwise, try to apply the variable elimination rule *)
      | None -> var_elimination_rule system sub)
    in


    (* CLASH *)
    let clash_rule system sub = 
      let clash_opt = EqSet.find_first_opt
        (fun (l,r) -> 
          match (l,r) with 
          | Arrow(_,_),Product(_,_) 
          | Arrow(_,_),Int
          | Product(_,_),Arrow(_,_) 
          | Product(_,_),Int        
          | Int,Product(_,_) 
          | Int,Arrow(_,_) -> true
          | (_,_) -> false ) system in

      (match clash_opt with 
      (* if a clash is found, the system is unsolvable *)
      | Some _  -> None 

      (* otherwise, try to apply the occur check rule *)
      | None    -> occur_check_rule system sub)
    in


    (* DECOMPOSITION *)
    let decomposition_rule system sub = 
      let decomp_opt = EqSet.find_first_opt 
        (fun (l,r) -> 
          match (l,r) with
          | Arrow(_,_),Arrow(_,_) -> true
          | Product(_,_),Product(_,_) -> true
          | (_,_) -> false) system in

      (match decomp_opt with
      | Some ((Arrow(l1,r1),Arrow(l2,r2)) as eq) 
      | Some ((Product(l1,r1),Product(l2,r2)) as eq) -> 
        (* remove the current equation *)
        let system = (EqSet.remove eq system) in
        (* add an equation l1 = l2 *)
        let system = (EqSet.add (l1,l2) system) in 
        (* add an equation r1 = r2 *)
        let system = (EqSet.add (r1,r2) system) in

        (* unification *)
        (match (Unification.unify (fst eq) (snd eq)) with 
        | Some sub2 -> aux system (TypeSubstitution.compose sub2 sub)
        | None      -> None)

      (* otherwise, try to apply the clash rule *)
      | _ -> clash_rule system sub )
    in
    
    (* Find the first equation that a rule can be applied to,
       removes it and applies the rule if such an equation exists,
       and calls aux recursively.
       returns the solution if no such equation exists *)
    decomposition_rule system sub
    
  in

  aux system (IdMap.empty)



let typeof (term : Term.t) =
  
  (* (1) For every new identifier and every expression, introduce a type variable *)
  let ty_vars = get_type_variables term in

  (* (2) Compute the equations *)
  let system = try 
    compute_system ty_vars term
  with 
    | _ -> EqSet.empty
  in

  if (system = EqSet.empty) then None else
  
  (* (3) solve the equations *)
  match (solve_system system) with 
  | Some sub  -> 
    let ty = ty_var_of_term ty_vars term in
    Some (TypeSubstitution.apply sub ty)
  | None      -> None

