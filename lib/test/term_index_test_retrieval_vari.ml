open Pgeon

(*
query: f(a, 'x, W.P(#0, ?Y, 'z), ?U)

variants: [
  f(a, 'z, W.P(#0, ?Y, 'x), ?U), (* F *)
  f(a, 'x, W.P(#0, ?Y, 'x), ?U), (* F *)
  f(a, 't, W.P(#0, ?Y, 't), ?U), (* F *)

  f(a, 'x, W.P(#0, ?U, 'z), ?Y), (* M *)
  f(a, 'x, W.P(#0, ?Q, 'z), ?U), (* M *)
  f(a, 'x, W.P(#0, ?Q, 'z), ?Q), (* M *)

  f(a, 't, W.P(#0, ?Q, 't), ?Q), (* F & M *)
  f(a, 'z, W.P(#0, ?U, 'x), ?Y), (* F & M *)
  f(a, 'x, W.P(#0, ?Y, 'x), ?Y), (* F & M *)
]
*)

let bvar = Term_utility.tbvar
let fvar = Term_utility.tfvar
let mvar = Term_utility.tmvar
let app = Term_utility.tapp
let bind = Term_utility.tbind
let const = Term_utility.tconst

let a = const "a"
let t = fvar "t"
let x = fvar "x"
let z = fvar "z"
let q = mvar "Q"
let u = mvar "U"
let y = mvar "Y"
let _0 = bvar 0
let w = bind "w"
let p = app "P"
let wp ts = w (p ts)
let f = app "f"

(* f(a, 'x, W.P(#0, ?Y, 'z), ?U) *)
let t_fq = f [a; x; wp [_0; y; z]; u]

(* f(a, 'z, W.P(#0, ?Y, 'x), ?U) *)
let t_f1 = f [a; z; wp [_0; y; x]; u]
(* f(a, 'x, W.P(#0, ?Y, 'x), ?U) *)
let t_f2 = f [a; x; wp [_0; y; x]; u]
(* f(a, 't, W.P(#0, ?Y, 't), ?U) *)
let t_f3 = f [a; t; wp [_0; y; t]; u]

(* f(a, 'x, W.P(#0, ?U, 'z), ?Y) *)
let t_f4 = f [a; x; wp [_0; u; z]; y]
(* f(a, 'x, W.P(#0, ?Q, 'z), ?U) *)
let t_f5 = f [a; x; wp [_0; q; z]; u]
(* f(a, 'x, W.P(#0, ?Q, 'z), ?Q) *)
let t_f6 = f [a; x; wp [_0; q; z]; q]

(* f(a, 't, W.P(#0, ?Q, 't), ?Q) *)
let t_f7 = f [a; t; wp [_0; q; t]; q]
(* f(a, 'z, W.P(#0, ?U, 'x), ?Y) *)
let t_f8 = f [a; z; wp [_0; u; x]; y]
(* f(a, 'x, W.P(#0, ?Y, 'x), ?Y) *)
let t_f9 = f [a; x; wp [_0; y; x]; y]

let t_fnot = f [x; x; x]

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
  t_fnot;
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
  List.nth terms 10;
]

(* let () =
  Printf.printf "indexed_terms: {\n%s\n}\n"
    (String.concat "\n" (List.map Term.string_of_full indexed_terms))
;; *)

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
let variants = Term_index.retrieve_variants index query (* (Term_index.make_options ~fvar:true ~mvar:false) *)

(* let () =
  Printf.printf "index: \n%s\n\n" (Term_index.string_of index);
  Printf.printf "variants: \n%s\n\n" (Term_index.string_of_term_set_full ~sep:"\n" variants);
;; *)

(* Instances with fvar contains f1 *)
let _ = assert( Term_index.TermSet.mem i_f1 variants )

(* Instances with fvar contains f2 *)
let _ = assert( Term_index.TermSet.mem i_f2 variants )

(* Instances with fvar contains f3 *)
let _ = assert( Term_index.TermSet.mem i_f3 variants )

(* Instances with mvar contains f4 *)
let _ = assert( Term_index.TermSet.mem i_f4 variants )

(* Instances with mvar contains f5 *)
let _ = assert( Term_index.TermSet.mem i_f5 variants )

(* Instances with mvar contains f6 *)
let _ = assert( Term_index.TermSet.mem i_f6 variants )

(* Instances with both contains f7 *)
let _ = assert( Term_index.TermSet.mem i_f7 variants )

(* Instances with both contains f8 *)
let _ = assert( Term_index.TermSet.mem i_f8 variants )

(* Instances with both contains f9 *)
let _ = assert( Term_index.TermSet.mem i_f9 variants )
