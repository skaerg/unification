open TypeInference
open Format


let main () = 

  let x = Identifier.Id "x" in
  let y = Identifier.Id "y" in
  let z = Identifier.Id "z" in
  let f = Identifier.Id "f" in
  let g = Identifier.Id "g" in
  let h = Identifier.Id "h" in

  let ls = [

    Term.IntConst 0;

    Term.(Fun (x, Fun (y, Fun (z, App((App (Var x, Var y)), Var z)))));

    (Term.App (Term.Fun (x, Term.Fun (y, 
      Term.Binop 
        (Term.Binop (Term.Binop (Term.Var x, Term.Plus, Term.Var y),Term.Times,Term.Var x),
      Term.Plus,
      Term.Binop (Term.Binop (Term.Binop (Term.Var x, Term.Minus, Term.Var y),Term.Times,Term.Var y),
      Term.Minus,Term.Var x)))),Term.IntConst 10));

    Term.App (Term.Fun (f, Term.Fun (x,
      Term.Binop (Term.App (Term.Var f,Term.Var x),
      Term.Plus,
      Term.IntConst 1))),
    Term.Fun (y,
      Term.Binop (
      Term.Var y,
      Term.Times,
      Term.IntConst 2)));
    
    Term.App (Term.Fun (f,Term.Fun (x,
      Term.Binop (Term.App (Term.Var f,Term.Var x),
      Term.Plus,
      Term.IntConst 1))),
    Term.Fun (y,
      Term.Binop (
        Term.Var y,
        Term.Times,
        Term.Binop (
          Term.IntConst 3,
          Term.Div,
          Term.IntConst 2))));

    Term.(Fun (x, Proj(First, Var x)));

    Term.Proj(First, IntConst 7);

    Term.(Fun (x, 
    Binop(Binop(
      Binop(Proj(First, Proj(First, Var x)), Times, Proj(First, Proj(Second, Var x))), 
      Minus, 
      Proj(Second, Proj(First, Var x))), Plus, Proj(Second, Proj(Second, Var x))) ));

    Term.(Fun (x, Fun (y, Fun (z, App (App((Var x),App(Var y, Var z)),Pair(Var y, Var z))))));

    Term.Fun(f, Binop((App((Var f), IntConst 5)),Plus,(App((Var f), Pair(IntConst 3, IntConst 4)))));

    Term.Fun(f, Binop((App((Var f), IntConst 36)),Plus,(App((Var f), IntConst 12))));

    Term.Fun(g, Fun(h, Fun(x, App((Var h), App((Var g),(Var x))))))
    
  ] in

  print_string "Projet PFA 2023-2024\n\nHere is the output of our typeof function when applied to a few different terms : \n\n\n";


  List.iter (fun term -> 

    let ty_str = match (Inference.typeof term) with
    | Some ty -> (Type.to_string ty)
    | None    -> "ill-typed" in

    print_string ("typeof \t" ^ (Term.to_string term) ^ " : \n\t" ^ ty_str ^ "\n\n"); 
    ) ls;
  
;;

main ()

