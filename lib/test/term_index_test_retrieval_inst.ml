(* Unit tests for Term_index.retrieve_instances.
Does not test handling of nonlinearity,
nor terms that should not be in the result *)

open Pgeon

(*
query: f(a, W.P('x, ?Y, b), 'z)
instances: [
  f(a, W.P(a, ?Y, b), 'z), (* F *)
  f(a, W.P(a, ?Y, b), c), (* F *)
  f(a, W.P('x, ?Y, b), c), (* F *)
  f(a, W.P(g(?Y, c, 'z), ?Y, b), g(?Y, c, 'x)), (* F *)

  f(a, W.P('x, K.#1, b), 'z), (* M *)
  f(a, W.P('x, g(?Y, c, 'z), b), 'z), (* M *)
  f(a, W.P('x, W.'z, b), 'z), (* M *)

  f(a, W.P(a, a, b), a), (* F & M *)
  f(a, W.P(K.#1, K.#1, b), ?Y), (* F & M *)
  f(a, W.P('x, c, b), 'z), (* F & M *)
  f(a, W.P('x, W.P(?Y, #0, 'z), b), 'z), (* F & M *)
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

(* f(a, W.P('x, ?Y, b), 'z) *)
let t_fq = f [a; wp [x; y; b]; z]

(* f(a, W.P(a, ?Y, b), 'z) *)
let t_f1 = f [a; wp [a; y; b]; z]

(* f(a, W.P(a, ?Y, b), c) *)
let t_f2 = f [a; wp [a; y; b]; c]

(* f(a, W.P('x, ?Y, b), c) *)
let t_f3 = f [a; wp [x; y; b]; c]

(* f(a, W.P(g(?Y, c, 'z), ?Y, b), g(?Y, c, 'x)) *)
let t_f4 = f [a; wp [g [y; c; z]; y; b]; g [y; c; x]]

(* f(a, W.P('x, K.#1, b), 'z) *)
let t_f5 = f [a; wp [x; k _1; b]; z]

(* f(a, W.P('x, g(?Y, c, 'z), b), 'z) *)
let t_f6 = f [a; wp [x; g [y; c; z]; b]; z]

(* f(a, W.P('x, W.'z, b), 'z) *)
let t_f7 = f [a; wp [x; w z; b]; z]

(* f(a, W.P(a, a, b), a) *)
let t_f8 = f [a; wp [a; a; b]; a]

(* f(a, W.P(K.#1, K.#1, b), ?Y) *)
let t_f9 = f [a; wp [k _1; k _1; b]; y]

(* f(a, W.P('x, c, b), 'z) *)
let t_f10 = f [a; wp [x; c; b]; z]

(* f(a, W.P('x, W.P(?Y, #0, 'z), b), 'z) *)
let t_f11 = f [a; wp [x; wp [y; _0; z]; b]; z]

let templates = [
  t_fq;
  t_f1;
  t_f2;
  t_f3;
  t_f4;
  t_f5;
  t_f6;
  t_f7;
  t_f8;
  t_f9;
  t_f10;
  t_f11;
]

let (terms, _factory1) = Term_utility.create_many (List.to_seq templates) Term.empty_factory

let indexed_terms = Seq.drop 1 (List.to_seq terms)

(* let () =
  Printf.printf "indexed_terms: {\n%s\n}\n"
    (String.concat "\n" (List.map Term.string_of_full indexed_terms))
;; *)

let query = List.nth terms 0
let i_f1 = List.nth terms 1
let i_f2 = List.nth terms 2
let i_f3 = List.nth terms 3
let i_f4 = List.nth terms 4
let i_f5 = List.nth terms 5
let i_f6 = List.nth terms 6
let i_f7 = List.nth terms 7
let i_f8 = List.nth terms 8
let i_f9 = List.nth terms 9
let i_f10 = List.nth terms 10
let i_f11 = List.nth terms 11

let index = Term_index.add_terms Term_index.empty indexed_terms
let instances_f = Term_index.retrieve_instances index query (Term.make_substitutability ~fvar:true ~mvar:false)
let instances_m = Term_index.retrieve_instances index query (Term.make_substitutability ~fvar:false ~mvar:true)
let instances_fm = Term_index.retrieve_instances index query (Term.make_substitutability ~fvar:true ~mvar:true)

(* let () =
  Printf.printf "index: \n%s\n\n" (Term_index.string_of index);
  Printf.printf "instances_f: \n%s\n\n" (Term_index.string_of_term_set_full ~sep:"\n" instances_f);
  Printf.printf "instances_m: \n%s\n\n" (Term_index.string_of_term_set_full ~sep:"\n" instances_m);
  Printf.printf "instances_fm: \n%s\n\n" (Term_index.string_of_term_set_full ~sep:"\n" instances_fm);
;; *)

(* Instances with fvar contains f1 *)
let%test _ = Term_index.TermSet.mem i_f1 instances_f

(* Instances with fvar contains f2 *)
let%test _ = Term_index.TermSet.mem i_f2 instances_f

(* Instances with fvar contains f3 *)
let%test _ = Term_index.TermSet.mem i_f3 instances_f

(* Instances with fvar contains f4 *)
let%test _ = Term_index.TermSet.mem i_f4 instances_f

(* Instances with mvar contains f5 *)
let%test _ = Term_index.TermSet.mem i_f5 instances_m

(* Instances with mvar contains f6 *)
let%test _ = Term_index.TermSet.mem i_f6 instances_m

(* Instances with mvar contains f7 *)
let%test _ = Term_index.TermSet.mem i_f7 instances_m

(* Instances with both contains f8 *)
let%test _ = Term_index.TermSet.mem i_f8 instances_fm

(* Instances with both contains f9 *)
let%test _ = Term_index.TermSet.mem i_f9 instances_fm

(* Instances with both contains f10 *)
let%test _ = Term_index.TermSet.mem i_f10 instances_fm

(* Instances with both contains f11 *)
let%test _ = Term_index.TermSet.mem i_f11 instances_fm

(* Instances with fvar are all in instances with both *)
let%test _ = Term_index.TermSet.subset instances_f instances_fm

(* Instances with mvar are all in instances with both *)
let%test _ = Term_index.TermSet.subset instances_m instances_fm
