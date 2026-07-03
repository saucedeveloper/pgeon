(* Demonstrates retrieval algorithms *)

(* pgeon/lib $
ocamlc -c utils.mli term.mli term_symbol.mli term_index.mli
*)
(* pgeon/lib $
ocamlc -o index_retrieval_demo.exe utils.ml term.ml term_symbol.ml term_index.ml demo/index_retrieval_demo.ml
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
    tapp "f" [b; y];
    tapp "f" [a; c];
    a;
    b;
    c;
    x;
    y;
  ] in
  let (term_list, _factory1) = create_many templates factory0 in
  let terms = Array.of_list term_list in
  let indexed_terms = [
    terms.(0);
    terms.(1);
    terms.(2);
    terms.(3);
    terms.(4);
    terms.(8);
  ] in

  let index = Term_index.empty in
  let inserted_index = List.fold_left Term_index.add_term index indexed_terms in

  Printf.printf "index: %s\n\n" (Term_index.string_of inserted_index);
  Printf.printf "indexed terms: %s\n\n" (String.concat ", " (List.map Term_utility.string_of_full indexed_terms));

  let options: Term_index.retrieval_options = {
    fvar_instantiable = true;
    mvar_instantiable = true;
  } in

  let query = terms.(0) in

  let generalizations = Term_index.retrieve_generalizations inserted_index query options in
  Printf.printf "query: %s\ngeneralizations: %s\n\n"
    (Term.string_of query)
    (Term_index.string_of_term_set_full generalizations);

  let query = terms.(1) in

  let instances = Term_index.retrieve_instances inserted_index query options in
  Printf.printf "query: %s\ninstances: %s\n"
    (Term.string_of query)
    (Term_index.string_of_term_set_full instances);

  ;;
