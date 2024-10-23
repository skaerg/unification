

(* Fonction pour unifier deux types *)
let unify (t1 : Type.t) (t2 : Type.t) : TypeSubstitution.t option =
  let rec unify_rec (subst : TypeSubstitution.t) (ty1 : Type.t) (ty2 : Type.t) : TypeSubstitution.t option =
    match (ty1, ty2) with
    | Var id1, Var id2 when Identifier.equal id1 id2 -> Some subst
    | Var id, ty | ty, Var id ->

    (* On vérifie s'il y a déjà (id, ty') dans subst *)
    (match TypeSubstitution.IdMap.find_opt id subst with
      | Some ty' ->
          (* On essaye d'unifier ty et ty' *)
          (match unify_rec subst ty' ty with
          | Some subst' -> Some subst'
          | None -> None)
      | None ->
          (* S'il n'existe pas de binding pour ty id alors on en créée un *)
          if Type.is_free ty id && Type.Var id <> ty then None
          else let subst' = TypeSubstitution.IdMap.add id ty subst in
          Some subst')
    | Int, Int -> Some subst
    | Product (tA, tB), Product (tA1, tB1)
    | Arrow (tA, tB), Arrow (tA1, tB1) ->
        (* On unifie les premiers termes des deux opérations *)
        (match unify_rec subst tA tA1 with
        (* On continue avec les deuxièmes s'il y a une substitution *)
        | Some subst' -> unify_rec subst' tB tB1
        | None -> None)
    | _ -> None
  in
  unify_rec TypeSubstitution.IdMap.empty t1 t2