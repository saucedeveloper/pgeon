open Pgeon

(*
query: f(a, W.P(b, #0, 'x, K.#1), g(?Y, c, 'z))
generalizations: [
  'x, (* F *)
  'Y, (* M *)
  f(a, 'z, 'x), (* F *)
  f('x, ?Y, 'z), (* F & M *)
  f(a, W.'z, g(?Y, c, 'x)), (* F & M *)
  f('x, W.P(?Y, #0, 'z, K.#1), 'x), (* None *)
]
*)
let t_a = Term_utility.tconst "a"
let t_b = Term_utility.tconst "b"
let t_c = Term_utility.tconst "c"
let t_0 = Term_utility.tbvar 0
let t_1 = Term_utility.tbvar 1
let t_x = Term_utility.tfvar "x"
let t_z = Term_utility.tfvar "z"
let t_Y = Term_utility.tmvar "Y"
let t_K = Term_utility.tbind "K" t_1
let t_g = Term_utility.tapp "g" [t_Y; t_c; t_z]
let t_g1 = Term_utility.tapp "g" [t_Y; t_c; t_x]
let t_P = Term_utility.tapp "P" [t_b; t_0; t_x; t_K]
let t_P2 = Term_utility.tapp "P" [t_Y; t_0; t_z; t_K]
let t_W = Term_utility.tbind "W" t_P
let t_W1 = Term_utility.tbind "W" t_z
let t_W2 = Term_utility.tbind "W" t_P2
let t_f = Term_utility.tapp "f" [t_a; t_W; t_g]
let t_f1 = Term_utility.tapp "f" [t_a; t_z; t_x]
let t_f2 = Term_utility.tapp "f" [t_x; t_Y; t_z]
let t_f3 = Term_utility.tapp "f" [t_a; t_W1; t_g1]
let t_f4 = Term_utility.tapp "f" [t_x; t_W2; t_x]

let templates = [
  t_f;
  t_x;
  t_Y;
  t_f1;
  t_f2;
  t_f3;
  t_f4;
  t_a;
  t_b;
  t_c;
  t_0;
  t_1;
  t_z;
  t_K;
  t_g;
  t_g1;
  t_P;
  t_P2;
  t_W;
  t_W1;
  t_W2;
]

let (terms, _factory1) = Term_utility.create_many (List.to_seq templates) Term.empty_factory
(* let indexed_terms = List.take 6 (List.drop 1 terms) *)
let indexed_terms = [
  List.nth terms 1;
  List.nth terms 2;
  List.nth terms 3;
  List.nth terms 4;
  List.nth terms 5;
  List.nth terms 6;
]
let i_x = List.nth indexed_terms 0
let i_Y = List.nth indexed_terms 1
let i_f1 = List.nth indexed_terms 2
let i_f2 = List.nth indexed_terms 3
let i_f3 = List.nth indexed_terms 4
let i_f4 = List.nth indexed_terms 5
let query = List.hd terms
let index = Term_index.add_terms Term_index.empty (List.to_seq indexed_terms)
let generalizations_f = Term_index.retrieve_generalizations index query (Term.make_substitutability ~fvar:true ~mvar:false)
let generalizations_m = Term_index.retrieve_generalizations index query (Term.make_substitutability ~fvar:false ~mvar:true)
let generalizations_fm = Term_index.retrieve_generalizations index query (Term.make_substitutability ~fvar:true ~mvar:true)

(* Generalizations with fvar contains 'x *)
let%test _ = Term_index.TermSet.mem i_x generalizations_f

(* Generalizations with fvar contains f(a, 'z, 'x) *)
let%test _ = Term_index.TermSet.mem i_f1 generalizations_f

(* Generalizations with mvar contains ?Y *)
let%test _ = Term_index.TermSet.mem i_Y generalizations_m

(* Generalizations with both contains 'x *)
let%test _ = Term_index.TermSet.mem i_x generalizations_fm

(* Generalizations with both contains ?Y *)
let%test _ = Term_index.TermSet.mem i_Y generalizations_fm

(* Generalizations with both contains f(a, 'z, 'x) *)
let%test _ = Term_index.TermSet.mem i_f1 generalizations_fm

(* Generalizations with both contains f('x, ?Y, 'z) *)
let%test _ = Term_index.TermSet.mem i_f2 generalizations_fm

(* Generalizations with both contains f(a, W.'z, g(?Y, c, 'x)) *)
let%test _ = Term_index.TermSet.mem i_f3 generalizations_fm

(* Generalizations with both does not contain f('x, W.P(?Y, #0, 'z, K.#1), 'x)) *)
let%test _ = not (Term_index.TermSet.mem i_f4 generalizations_fm)

(* Generalizations with fvar are all in generalizations with both *)
let%test _ = Term_index.TermSet.subset generalizations_f generalizations_fm

(* Generalizations with mvar are all in generalizations with both *)
let%test _ = Term_index.TermSet.subset generalizations_m generalizations_fm
