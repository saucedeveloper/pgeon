(* Unit tests for Term_index.retrieve_generalizations.
Does not test handling of nonlinearity *)

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

let bvar = Term_utility.tbvar
let fvar = Term_utility.tfvar
let mvar = Term_utility.tmvar
let app = Term_utility.tapp
let bind = Term_utility.tbind
let const = Term_utility.tconst

let a = const "a"
let b = const "b"
let c = const "c"
let _0 = bvar 0
let _1 = bvar 1
let x = fvar "x"
let z = fvar "z"
let y = mvar "Y"
let k = bind "K"
let p = app "P"
let g = app "g"
let w = bind "W"
let wp ts = w (p ts)
let f = app "f"

(* f(a, W.P(b, #0, 'x, K.#1), g(?Y, c, 'z)) *)
let t_fq = f [a; wp [b; _0; x; k _1]; g [y; c; z]]

(* f(a, 'z, 'x) *)
let t_f1 = f [a; z; x]

(* f('x, ?Y, 'z) *)
let t_f2 = f [x; y; z]

(* f(a, W.'z, g(?Y, c, 'x)) *)
let t_f3 = f [a; w z; g [y; c; x]]

(* f('x, W.P(?Y, #0, 'z, K.#1), 'x) *)
let t_f4 = f [x; wp [y; _0; z; k _1]; x]

let templates = [
  t_fq;
  x;
  y;
  t_f1;
  t_f2;
  t_f3;
  t_f4;
]

let (terms, _factory1) = Term_utility.create_many (List.to_seq templates) Term.empty_factory

let indexed_terms = Seq.drop 1 (List.to_seq terms)

let i_x = List.nth terms 1
let i_Y = List.nth terms 2
let i_f1 = List.nth terms 3
let i_f2 = List.nth terms 4
let i_f3 = List.nth terms 5
let i_f4 = List.nth terms 6
let query = List.hd terms
let index = Term_index.add_terms Term_index.empty indexed_terms
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
