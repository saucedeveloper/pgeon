open Pgeon

(*
query: f(a, W.P(b, ?Y, #0), 'z)

unifiable: [
  f(a, 'x, 't), (* F *)
  f('t, 'x, 't), (* F *)
  f(a, 'x, ?U), (* F *)

  f(a, ?U, 'z), (* M *)
  f(?U, W.P(b, a, #0), 'z), (* M *)
  f(a, W.P(b, 't, #0), 'z), (* M *)

  f('x, W.P('t, b, #0), c), (* F & M *)
  f(a, W.P(b, b, #0), ?U), (* F & M *)
  f('x, ?U, a), (* F & M *)
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
let t = fvar "t"
let x = fvar "x"
let z = fvar "z"
let u = mvar "U"
let y = mvar "Y"
let _0 = bvar 0
let w = bind "w"
let p = app "P"
let wp ts = w (p ts)
let f = app "f"

(* f(a, W.P(b, ?Y, c), 'z) *)
let t_fq = f [a; wp [b; y; _0]; z]

let t_f1 = f [a; x; t] (* f(a, 'x, 't) *)
let t_f2 = f [t; x; t]  (* f('t, 'x, 't) *)
let t_f3 = f [a; x; u] (* f(a, 'x, ?U) *)

let t_f4 = f [a; u; z] (* f(a, ?U, 'z) *)
let t_f5 = f [u; wp [b; a; _0]; z] (* f(?U, W.P(b, a, c), 'z) *)
let t_f6 = f [a; wp [b; t; _0]; z] (* f(a, W.P(b, 't, c), 'z) *)

let t_f7 = f [x; wp [t; b; _0]; c]  (* f('x, W.P('t, b, c), c) *)
let t_f8 = f [a; wp [b; b; _0]; u] (* f(a, W.P(b, b, c), ?U) *)
let t_f9 = f [x; u; a] (* f('x, ?U, a) *)

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
]

let (terms, _factory1) = Term_utility.create_many (List.to_seq templates) Term.empty_factory
let indexed_terms = [
  List.nth terms 1;
  List.nth terms 2;
  List.nth terms 3;
  List.nth terms 4;
  List.nth terms 5;
  List.nth terms 6;
  List.nth terms 7;
  List.nth terms 8;
  List.nth terms 9;
]

let i_f1 = List.nth indexed_terms 0
let i_f2 = List.nth indexed_terms 1
let i_f3 = List.nth indexed_terms 2
let i_f4 = List.nth indexed_terms 3
let i_f5 = List.nth indexed_terms 4
let i_f6 = List.nth indexed_terms 5
let i_f7 = List.nth indexed_terms 6
let i_f8 = List.nth indexed_terms 7
let i_f9 = List.nth indexed_terms 8
let query = List.hd terms
let index = Term_index.add_terms Term_index.empty (List.to_seq indexed_terms)
let unifiable_f = Term_index.retrieve_unifiable index query (Term.make_substitutability ~fvar:true ~mvar:false)
let unifiable_m = Term_index.retrieve_unifiable index query (Term.make_substitutability ~fvar:false ~mvar:true)
let unifiable_fm = Term_index.retrieve_unifiable index query (Term.make_substitutability ~fvar:true ~mvar:true)

(* Instances with fvar contains f1 *)
let%test _ = Term_index.TermSet.mem i_f1 unifiable_f

(* Instances with fvar contains f2 *)
let%test _ = Term_index.TermSet.mem i_f2 unifiable_f

(* Instances with fvar contains f3 *)
let%test _ = Term_index.TermSet.mem i_f3 unifiable_f

(* Instances with mvar contains f4 *)
let%test _ = Term_index.TermSet.mem i_f4 unifiable_m

(* Instances with mvar contains f5 *)
let%test _ = Term_index.TermSet.mem i_f5 unifiable_m

(* Instances with mvar contains f6 *)
let%test _ = Term_index.TermSet.mem i_f6 unifiable_m

(* Instances with both contains f7 *)
let%test _ = Term_index.TermSet.mem i_f7 unifiable_fm

(* Instances with both contains f8 *)
let%test _ = Term_index.TermSet.mem i_f8 unifiable_fm

(* Instances with both contains f9 *)
let%test _ = Term_index.TermSet.mem i_f9 unifiable_fm

(* Instances with fvar are all in unifiable with both *)
let%test _ = Term_index.TermSet.subset unifiable_f unifiable_fm

(* Instances with mvar are all in unifiable with both *)
let%test _ = Term_index.TermSet.subset unifiable_m unifiable_fm
