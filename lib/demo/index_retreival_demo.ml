(* pgeon/lib $
ocamlc -c utils.mli term.mli term_symbol.mli term_index.mli
*)
(* pgeon/lib $
ocamlc -o index_retreival_demo.exe utils.ml term.ml term_symbol.ml term_index.ml demo/index_retreival_demo.ml
*)

(*
f(a, y, c) is a generalization of
{
  f(a, g(b, x), c),
  f(a, b, c),
  f(a, x, c),
}
but not
{
  f(b, y, c),
  f(a, y, a),
  f(a, y, g(x, b)),
}
*)

open Pgeon

let () =
  let factory0 = Term.empty_factory in
  let (a, factory1) = Term.create_const "a" factory0 in
  let (b, factory2) = Term.create_const "b" factory1 in
  let (c, factory3) = Term.create_const "c" factory2 in
  let (x, factory4) = Term.create_fvar  "x" factory3 in
  let (y, factory5) = Term.create_fvar  "y" factory4 in
  let (g1, factory6) = Term.create_app "g" [b; x] factory5 in
  let (g2, factory7) = Term.create_app "g" [x; b] factory6 in
  let (f_gen, factory8) = Term.create_app "f" [a; y; c] factory7 in
  let (f_y1, factory9) = Term.create_app "f" [a; g1; c] factory8 in
  let (f_y2, factory10) = Term.create_app "f" [a; b; c] factory9 in
  let (f_y3, factory11) = Term.create_app "f" [a; x; c] factory10 in
  let (f_n1, factory12) = Term.create_app "f" [b; y; c] factory11 in
  let (f_n2, factory13) = Term.create_app "f" [a; y; a] factory12 in
  let (f_n3, _factory14) = Term.create_app "f" [a; y; g2] factory13 in

  let f_terms = [f_y1; f_y2; f_y3; f_n1; f_n2; f_n3] in
  let index = Term_index.empty in
  let inserted_index = List.fold_left Term_index.add_term index f_terms in

  let generalizations = Term_index.retreive_generalizations inserted_index f_gen ~fvar_instanciable:true ~mvar_instanciable:false in
  Printf.printf "term: %s, generalizations: %s\n"
    (Term.string_of f_gen)
    (Term_index.string_of_term_set_full ~n:0 generalizations);

  ;;
