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

open Pgeon

let () =
  let factory0 = Term.empty_factory in
  let open Term_utility in
  let a = tconst "a" in
  let b = tconst "b" in
  let c = tconst "c" in
  let x = tfvar "x" in
  let y = tfvar "y" in
  let templates = [
    tapp "f" [a; b];
    tapp "f" [a; y];
    tapp "f" [x; y];
    a; b; c; x; y;
  ] in
  let (term_list, _factory1) = create_many templates factory0 in
  let terms = Array.of_list term_list in
  let indexed_terms = [
    terms.(0);
    terms.(1);
    terms.(2);
    terms.(6);
  ] in
  let query = terms.(0) in

  let index = Term_index.empty in
  let inserted_index = List.fold_left Term_index.add_term index indexed_terms in

  Printf.printf "index: %s\n" (Term_index.string_of inserted_index);
  Printf.printf "indexed terms: %s\n" (String.concat ", " (List.map Term_utility.string_of_full indexed_terms));

  let options: Term_index.retreival_options = {
    fvar_instanciable = true;
    mvar_instanciable = true;
  } in
  let generalizations = Term_index.retreive_generalizations inserted_index query options in
  Printf.printf "query: %s\ngeneralizations: %s\n"
    (Term.string_of query)
    (Term_index.string_of_term_set_full generalizations);

  ;;
