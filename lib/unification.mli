(*
   The function `unify` must compute the substitution
   `s` such that if  `unify t1 t2 = Some s` then
   `apply s t1 = apply s t2`.

   You can use the slides on the Herbrand / Robinson algorithm
   to start designing your implementation.

*)
val unify : Type.t -> Type.t -> TypeSubstitution.t option
