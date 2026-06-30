(* Demonstrates retreival algorithms *)

(* pgeon/lib $
ocamlc -c utils.mli term.mli term_symbol.mli term_index.mli
*)
(* pgeon/lib $
ocamlc -o index_retreival_demo.exe utils.ml term.ml term_symbol.ml term_index.ml demo/index_retreival_demo.ml
*)

(*
Generalizations of f(a, y, c) are
{
  f(a, y, x),
  x,
  Z
}
but not
{
  f(a, g(b, x), c),
  f(a, b, c),
  f(b, y, c),
  f(a, y, a),
  f(a, y, g(x, b)),
}
*)

let () =
  let factory0 = Term.empty_factory in
  let (a, factory1) = Term.create_const "a" factory0 in
  let (b, factory2) = Term.create_const "b" factory1 in
  let (c, factory3) = Term.create_const "c" factory2 in
  let (x, factory4) = Term.create_fvar  "x" factory3 in
  let (y, factory5) = Term.create_fvar  "y" factory4 in
  let (g1, factory6) = Term.create_app "g" [b; x] factory5 in
  let (g2, factory7) = Term.create_app "g" [x; b] factory6 in
  let (f_gen, factory8) = Term.create_app "f" [a; b; c] factory7 in
  let (f1, factory9) = Term.create_app "f" [a; g1; c] factory8 in
  let (f2, factory10) = Term.create_app "f" [a; y; c] factory9 in
  let (f3, factory11) = Term.create_app "f" [a; y; x] factory10 in
  let (f4, factory12) = Term.create_app "f" [b; y; c] factory11 in
  let (f5, factory13) = Term.create_app "f" [a; y; a] factory12 in
  let (f6, factory14) = Term.create_app "f" [a; y; g2] factory13 in
  let (meta_z, factory15) = Term.create_mvar "Z" factory14 in

  let f_terms = [f1; f2; f3; f4; f5; f6] in
  let index = Term_index.empty in
  let inserted_index = List.fold_left Term_index.add_term index f_terms in

  (* Printf.printf "index: %s\n" (Term_index.string_of inserted_index); *)
  Printf.printf "terms: %s\n" (String.concat ", " (List.map Term.string_of f_terms));

  let options: Term_index.retreival_options = {
    fvar_instanciable = true;
    mvar_instanciable = true;
  } in
  let generalizations = Term_index.retreive_generalizations inserted_index f_gen options in
  Printf.printf "query: %s\ngeneralizations: %s\n"
    (Term.string_of f_gen)
    (Term_index.string_of_term_set_full ~n:0 generalizations);

  ;;
