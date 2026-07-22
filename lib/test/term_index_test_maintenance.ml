open Pgeon

let t_bvar = Term_utility.tbvar 0
let t_fvar = Term_utility.tfvar "x"
let t_mvar = Term_utility.tmvar "Y"
let t_const = Term_utility.tconst "a"
let t_appf = Term_utility.tapp "f" [t_bvar; t_const; t_fvar]
let t_appg = Term_utility.tapp "g" [t_mvar; t_appf]

(* W.g(?Y, f(#0, a, 'x)) *)
let t_bind = Term_utility.tbind "W" t_appg

let templates = [
  t_bind;
  t_bvar;
  t_fvar;
  t_mvar;
  t_const;
  t_appf;
  t_appg;
]

let (terms, _factory1) = Term_utility.create_many (List.to_seq templates) Term.empty_factory

let index_with_terms = Term_index.add_terms Term_index.empty (List.to_seq terms)
let index_with_terms_removed = Term_index.remove_terms index_with_terms (List.to_seq terms)

let%test _ = Term_index.contains_term index_with_terms (List.nth terms 0)
let%test _ = Term_index.contains_term index_with_terms (List.nth terms 1)
let%test _ = Term_index.contains_term index_with_terms (List.nth terms 2)
let%test _ = Term_index.contains_term index_with_terms (List.nth terms 3)
let%test _ = Term_index.contains_term index_with_terms (List.nth terms 4)
let%test _ = Term_index.contains_term index_with_terms (List.nth terms 5)
let%test _ = Term_index.contains_term index_with_terms (List.nth terms 6)
let%test _ = assert ((List.length templates) = 7); true
let%test _ = Term_index.contains_all_terms index_with_terms (List.to_seq terms)

(* index is empty *)
let%test _ = Term_index.is_empty index_with_terms_removed
