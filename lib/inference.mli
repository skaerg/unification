
module TermMap : Map.S with type key = Term.t
type t = Identifier.t TermMap.t


(* 
   If `t` is a term, then `typeof t` must compute
   - `None`, if there is no type for `t`
   - `Some ty`, if ty is the type of term `t`
*)
val typeof : Term.t -> Type.t option
    
