open TypeInference
open Alcotest
open TypeSubstitution
open Identifier



(* Testable modules *)


let identifierModule = (module Identifier : Alcotest.TESTABLE with type t = Identifier.t)
let typeModule = (module Type : Alcotest.TESTABLE with type t = Type.t)
let typeSubModule = (module TypeSubstitution : Alcotest.TESTABLE with type t = TypeSubstitution.t)




(* Tests for Identifier.fresh *)

let id_of_int (n : int) =
  Identifier.Id ("id" ^ (string_of_int (n)))

let tests_fresh = 
  List.init 5 (fun i -> (
    ("fresh identifier " ^ (string_of_int (i+1))),
    ( Some (Identifier.fresh ())),
    ( Some (id_of_int (i+1)))))

let check_fresh id_text id expected_id =
  test_case id_text `Quick (fun () -> check (option identifierModule) "Same identifier" id expected_id)



(* Tests for TypeSubstitution.apply *)

let tests_apply =

  let ty = Type.Product (
    Type.Var(Id "x"), 
    Type.Arrow((Var (Id "x")), (Var (Id "y")))) in

  let sub1 = bindings_to_subst
    [ ((Id "x"),(Type.Var (Id "z"))) ] in

  let expected1 = Type.Product (
    Type.Var(Id "z"), 
    Type.Arrow((Var (Id "z")), (Var (Id "y")))) in

  let sub2 = bindings_to_subst
    [ ((Id "x"),(Type.Var (Id "y"))) ] in

  let expected2 = Type.Product (
    Type.Var(Id "y"), 
    Type.Arrow((Var (Id "y")), (Var (Id "y")))) in
  
  let sub3 = bindings_to_subst
    [ ((Id "x"),(Type.Var (Id "z"))) ;
      ((Id "y"),(Type.Var (Id "z"))) ] in

  let expected3 = Type.Product (
    Type.Var(Id "z"), 
    Type.Arrow((Var (Id "z")), (Var (Id "z")))) in

  let sub4 = bindings_to_subst
    [ ((Id "x"),(Type.Var (Id "a"))) ;
      ((Id "y"),(Type.Product (Type.Var(Id "x"),Type.Var(Id "y")))) ] in

  let expected4 = Type.Product (
    Type.Var(Id "a"), 
    Type.Arrow((Var (Id "a")), (Type.Product 
      ( Type.Var(Id "x"),
        Type.Var(Id "y"))))) in

  [ ("x * (x * y)[x/z]", sub1, ty, (Some expected1)) ;
    ("x * (x * y)[x/y]", sub2, ty, (Some expected2)) ;
    ("x * (x * y)[x/z, y/z]", sub3, ty, (Some expected3)) ; 
    ("x * (x * y)[x/a, y/(x * y)]", sub4, ty, (Some expected4)) ;]

  

let check_apply ty_text sub ty expected_sub =
  test_case ty_text `Quick (fun () -> check (option typeModule) "Equal substitution" (Some (apply sub ty)) expected_sub)



(* Tests for TypeSubstitution.compose *)

let tests_compose = 

  let sigma = bindings_to_subst 
    [ ((Id "x"),(Type.Var (Id "y"))) ; 
      ((Id "w"),(Type.Product((Var (Id "z")), (Var (Id "z"))))) ] in

  let tau = bindings_to_subst
    [ ((Id "y"),(Type.Var (Id "a"))) ;
      ((Id "z"),(Type.Arrow ((Var (Id "x")), (Var (Id "b"))))) ] in
  
  (* substitution that contains exactly the expected bindings in tau ◦ sigma *)
  let tau_sigma_expected = bindings_to_subst 
    [ ((Id "x"),(Type.Var (Id "a"))) ;
      ((Id "y"),(Type.Var (Id "a"))) ;
      ((Id "z"),(Type.Arrow (Type.Var (Id "x"), Type.Var (Id "b")))) ;
      ((Id "w"),(Type.Product (
        (Type.Arrow (Type.Var (Id "x"), Type.Var (Id "b"))),
        (Type.Arrow (Type.Var (Id "x"), Type.Var (Id "b"))))))] in

  (* substitution that contains exactly the expected bindings in sigma ◦ tau *)
  let sigma_tau_expected = bindings_to_subst 
  [ ((Id "y"),(Type.Var (Id "a"))) ;
    ((Id "z"),(Type.Arrow (Type.Var (Id "y"), Type.Var (Id "b")))) ;
    ((Id "x"),(Type.Var (Id "y"))) ;
    ((Id "w"),(Type.Product (Type.Var (Id "z"), Type.Var (Id "z"))))] in

  [ ("{y/a, z/(x->b)} ◦ {x/y, w/(z*z)}", tau, sigma, (Some tau_sigma_expected)) ; 
    ("{x/y, w/(z*z)} ◦ {y/a, z/(x->b)}", sigma, tau, (Some sigma_tau_expected)) ]


let check_compose cmp_text s1 s2 expected_sub =
  test_case cmp_text `Quick (fun () -> check (option typeSubModule) "Equal substitutions" expected_sub (Some (compose s1 s2)))



(* Tests for Inference.typeof *)

let tests_typeof =
  let x = Identifier.fresh () in
  let y = Identifier.fresh () in
  let z = Identifier.fresh () in

  let a = Identifier.fresh () in
  let b = Identifier.fresh () in
  let c = Identifier.fresh () in
  let f = Identifier.fresh () in
  let g = Identifier.fresh () in
  let h = Identifier.fresh () in

  [ ("0", Term.IntConst 0, Some Type.Int);
    ("x", Term.Var x, Some (Type.Var a));
    ("x+y", Term.Binop (Var x, Plus, Var y), Some Type.Int) ;
    ("fun x -> fun y -> x+y", Term.(Fun (x, Fun (y, Binop (Var x, Plus, Var y)))), Some Type.(Arrow (Int, Arrow (Int, Int)))); 
    ("fst ((x+y),z)", Term.(Proj (First,Pair((Binop (Var x, Plus, Var y)), Var z))), Some Type.Int) ;

    ("snd (fun x -> x + x)", Term.Proj(Second, Fun(x, Binop(Var x, Plus, Var x))), None);
    ("fst (fun x -> x,x )", Term.Proj(First, Fun(x, Pair(Var x, Var x))), None);
    ("(fun x -> x + y) 5", Term.(App (Fun (x, Binop (Var x, Plus, Var y)), IntConst 5)), Some Type.Int);
    ("(fun x -> x + x) 8", Term.App((Fun(x, Binop(Var x, Plus, Var x))), IntConst 8), Some Type.Int) ;

    ("fun x -> (fst (fst x)) * (fst (snd x)) - (snd (fst x)) + (snd (snd x))", 
    Term.(Fun (x, 
    Binop(
      Binop(
        Binop(Proj(First, Proj(First, Var x)), Times, Proj(First, Proj(Second, Var x))), 
        Minus, 
        Proj(Second, Proj(First, Var x))), Plus, Proj(Second, Proj(Second, Var x))) )), 
    Some Type.(Arrow (Product(Product(Int, Int), Product(Int, Int)), Int)));

    ("fst ((fun x -> x,x) 9)", Term.(Proj(First, (App (Fun(x, Pair(Var x, Var x)), IntConst 9)))), Some Type.Int);

    ("fst 7", Term.Proj(First, IntConst 7), None); 
    ("snd (21 / 3)", Term.Proj(Second, Binop(IntConst 21, Div, IntConst 3)), None);
    ("13 + 2", Term.(Binop(IntConst 13, Div, IntConst 2)), Some Type.Int) ;
    ("(16,x)", Term.(Pair ((IntConst 16),(Var x))), Some Type.(Product(Int,Var a)));

    ("fun x -> fst x ", 
    Term.(Fun (x, Proj(First, Var x))), 
    Some Type.(Arrow ((Product(Var a, Var b)),Var a))) ;

    ("fun x -> fun y -> x y", 
    Term.(Fun (x, Fun (y, App(Var x, Var y)))),
    Some Type.(Arrow (Arrow (Var a, Var b), Arrow (Var a, Var b)))) ;

    ("fun x -> fun y -> fun z -> x y z",
    Term.(Fun (x, Fun (y, Fun (z, App((App (Var x, Var y)), Var z))))),
    Some (Type.Arrow (
      (Type.Arrow ((Type.Var a),
        (Type.Arrow ((Type.Var b),
          (Type.Var c))))),
      (Type.Arrow ((Type.Var a),
        (Type.Arrow ((Type.Var b),
          (Type.Var c))))))));

    ("fun z -> fun y -> fun x -> x y z",
    Term.(Fun (z, Fun (y, Fun (x, App((App (Var x, Var y)), Var z))))),
    Some (Type.Arrow (Type.Var a,
      Type.Arrow (Type.Var b,
      Type.Arrow
        (Type.Arrow (Type.Var b,
          Type.Arrow (Type.Var a,
          Type.Var c)),
        Type.Var c)))));

    ("fun x -> fun y -> fun z -> x (y z) (y, z)", 
    Term.(Fun (x, Fun (y, Fun (z, App (App((Var x),App(Var y, Var z)),Pair(Var y, Var z)))))), 
    Some (Type.Arrow
      (Type.Arrow (Type.Var a,
        Type.Arrow
          (Type.Product
            (Type.Arrow (Type.Var b,
              Type.Var a),
            Type.Var b),
          Type.Var c)),
      Type.Arrow
        (Type.Arrow (Type.Var b,
          Type.Var a),
        Type.Arrow (Type.Var b, Type.Var c))))) ;

    ("fun f -> (f 5) + (f (3,4))",
    Term.Fun(f, Binop((App((Var f), IntConst 5)),Plus,(App((Var f), Pair(IntConst 3, IntConst 4))))),
    None ) ;

    ("fun g -> fun h -> fun x -> h (g x)",
    Term.Fun(g, Fun(h, Fun(x, App((Var h), App((Var g),(Var x)))))),
    Some
    (Type.Arrow (
       (Type.Arrow ((Type.Var a),
          (Type.Var b))),
       (Type.Arrow (
          (Type.Arrow ((Type.Var b),
             (Type.Var c))),
          (Type.Arrow ((Type.Var a),
             (Type.Var c)))
          ))
       )));
    
    ]



(* Tests whether a given term is of the expected type *)
let check_typeof term_text term expected_type =

  let actual_type = Inference.typeof term in

  (* actual_type_alpha is exactly equal to expected_type if and only if 
     expected_type and actual_type are alpha-equivalent. 
     Otherwise, actual_type_alpha is equal to actual_type. *)
  let actual_type_alpha = 
    match actual_type,expected_type with 
    | Some aty, Some ety  -> 
      if (Type.alpha_eq aty ety) then expected_type else actual_type
    | _,_               -> actual_type
  in

  (* Test expected_type against actual_type_alpha to check that they are equal *)
  test_case term_text `Quick (fun () -> check (option typeModule) "Equal types" (expected_type) actual_type_alpha)



(* Tests for Unification.unify *)
let tests_unify = 
  let t1 = Type.Product (Type.Int, Type.Var (Id "x")) in
  let t2 = Type.Product (Type.Var (Id "y"), Type.Int) in
  let t3 = Type.Arrow (Type.Int, Type.Int) in
  let t4 = Type.Product (Type.Var (Id "a"), (Type.Product (Type.Var (Id "b"), Type.Var (Id "c")) )) in
  let t5 = Type.Product (Type.Int, (Type.Var (Id "a"))) in
  let t6 = Type.Var (Id "a") in
  let t7 = Type.Product (Type.Var (Id "a"), Type.Var (Id "b")) in
  let t8 = Type.Product (Type.Var (Id "c"), Type.Var (Id "b")) in
  let t9 = Type.Arrow (Type.Var (Id "a"), Type.Var (Id "b")) in
  let t10 = Type.Arrow (t7, Type.Var (Id "c")) in
  let t11 = Type.Arrow (t9, Type.Var (Id "c")) in

  let sub1_2 = bindings_to_subst 
    [ ((Id "x"),Type.Int) ;
      ((Id "y"),Type.Int) ; ] in

  let sub6_8 = bindings_to_subst
    [ ((Id "a"),Type.Product (Type.Var (Id "c"), Type.Var (Id "b"))) ; ] in

  [ ("unify (int * x) (y * int)", t1, t2, Some sub1_2 ) ;
    ("unify (int * x) (int -> int)", t1, t3, None ) ;
    ("unify (a * (b * c)) (int * a)", t4, t5, None ) ;
    ("unify (a) (a * b)", t6, t7, None) ; 
    ("unify (a) (b * c)", t6, t8, Some sub6_8) ; 
    ("unify ((a * b) -> c) ((a -> b) -> c)" ), t10, t11, None ;
    ("unify int (a -> b)", Type.Int, t9, None) ;
    ("unify (a * b) int", t7, Type.Int, None) ;]


(* Check that unify ty1 ty2 is equal to the expected_substitution *)
let check_unify1 u_text ty1 ty2 expected_sub =
  test_case u_text `Quick (fun () -> check (option typeSubModule) "Equal substitutions" expected_sub (Unification.unify ty1 ty2))


(* Checks that given a substitution s and two types t1 and t2 such that if `unify t1 t2 = Some s`, then apply s t1 = apply s t2 *)
let check_unify2 u_text t1 t2 s =
  test_case u_text `Quick (fun () -> check (typeModule) "Equal types" (apply s t1) (apply s t2))


let () =
  run "Inference" [
      "fresh", List.map (fun (id_text, id, expected_id) -> check_fresh id_text id expected_id) tests_fresh;
      "apply", List.map (fun (ty_text, sub, ty, expected_type) -> check_apply ty_text sub ty expected_type) tests_apply ;
      "compose", List.map (fun (sub_text, s1, s2, expected_sub) -> check_compose sub_text s1 s2 expected_sub) tests_compose ;
      "unify 1", List.map (fun (u_text, ty1, ty2, expected_sub) -> check_unify1 u_text ty1 ty2 expected_sub) tests_unify ;
      "unify 2", List.map (fun (u_text, ty1, ty2, s) -> check_unify2 u_text ty1 ty2 s) 
        (List.filter_map (fun (text,t1,t2,s_opt) -> match s_opt with
          | Some s  -> Some (text,t1,t2,s)
          | None    -> None ) (tests_unify)) ;
      "typeof", List.map (fun (term_text, term, expected_type) -> check_typeof term_text term expected_type) tests_typeof ;
    ]
