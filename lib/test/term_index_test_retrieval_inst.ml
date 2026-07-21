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

let t_a = Term_utility.tconst "a"  (* a *)
let t_b = Term_utility.tconst "b"  (* b *)
let t_c = Term_utility.tconst "c"  (* c *)
let t_0 = Term_utility.tbvar 0  (* #0 *)
let t_1 = Term_utility.tbvar 1  (* #1 *)
let t_x = Term_utility.tfvar "x"  (* 'x *)
let t_z = Term_utility.tfvar "z"  (* 'z *)
let t_Y = Term_utility.tmvar "Y"  (* ?Y *)
let t_K = Term_utility.tbind "K" t_1  (* K.#1 *)
let t_g1 = Term_utility.tapp "g" [t_Y; t_c; t_z]  (* g(?Y, c, 'z) *)
let t_g2 = Term_utility.tapp "g" [t_Y; t_c; t_x]  (* g(?Y, c, 'x) *)
let t_P10 = Term_utility.tapp "P" [t_Y; t_0; t_z]  (* P(?Y, #0, 'z) *)
let t_W12 = Term_utility.tbind "W" t_z  (* W.'z *)
let t_W13 = Term_utility.tbind "W" t_P10  (* W.P(...) *)
let t_Pq = Term_utility.tapp "P" [t_x; t_Y; t_b]  (* P('x, ?Y, b) *)
let t_P1 = Term_utility.tapp "P" [t_a; t_Y; t_b]  (* P(a, ?Y, b) *)
let t_P2 = Term_utility.tapp "P" [t_g1; t_Y; t_b]  (* P(g(...), ?Y, b) *)
let t_P3 = Term_utility.tapp "P" [t_x; t_K; t_b]  (* P('x, K.#1, b) *)
let t_P4 = Term_utility.tapp "P" [t_x; t_g1; t_b]  (* P('x, g(...), b) *)
let t_P5 = Term_utility.tapp "P" [t_x; t_W12; t_b]  (* P('x, W.'z, b) *)
let t_P6 = Term_utility.tapp "P" [t_a; t_a; t_b]  (* P(a, a, b) *)
let t_P7 = Term_utility.tapp "P" [t_K; t_K; t_b]  (* P(K.#1, K.#1, b) *)
let t_P8 = Term_utility.tapp "P" [t_x; t_c; t_b]  (* P('x, c, b) *)
let t_P9 = Term_utility.tapp "P" [t_x; t_W13; t_b]  (* P('x, W.P(...), b) *)
let t_Wq = Term_utility.tbind "W" t_Pq  (* W.P('x, ?Y, b) *)
let t_W1 = Term_utility.tbind "W" t_P1  (* W.P(a, ?Y, b) *)
let t_W2 = Term_utility.tbind "W" t_P2  (* W.P(g(...), ?Y, b) *)
let t_W3 = Term_utility.tbind "W" t_P3  (* W.P('x, K.#1, b) *)
let t_W4 = Term_utility.tbind "W" t_P4  (* W.P('x, g(...), 'z) *)
let t_W5 = Term_utility.tbind "W" t_P5  (* W.P('x, W.'z, b) *)
let t_W6 = Term_utility.tbind "W" t_P6  (* W.P(a, a, b) *)
let t_W7 = Term_utility.tbind "W" t_P7  (* W.P(K.#1, K.#1, b) *)
let t_W8 = Term_utility.tbind "W" t_P8  (* W.P('x, c, b) *)
let t_W9 = Term_utility.tbind "W" t_P9  (* W.P('x, W.P(...), b) *)
let t_fq = Term_utility.tapp "f" [t_a; t_Wq; t_z]  (* f(a, W.P(...), 'z) *)
let t_f1 = Term_utility.tapp "f" [t_a; t_W1; t_z]  (* f(a, W.P(...), 'z) *)
let t_f2 = Term_utility.tapp "f" [t_a; t_W1; t_c]  (* f(a, W.P(...), c) *)
let t_f3 = Term_utility.tapp "f" [t_a; t_Wq; t_c]  (* f(a, W.P(...), c) *)
let t_f4 = Term_utility.tapp "f" [t_a; t_W2; t_g2]  (* f(a, W.P(g(?Y, c, 'z), ?Y, b), g(?Y, c, 'x)) *)
let t_f5 = Term_utility.tapp "f" [t_a; t_W3; t_z]  (* f(a, W.P('x, K.#1, b), 'z) *)
let t_f6 = Term_utility.tapp "f" [t_a; t_W4; t_z]  (* f(a, W.P('x, g(?Y, c, 'z), 'z), 'z) *)
let t_f7 = Term_utility.tapp "f" [t_a; t_W5; t_z]  (* f(a, W.P('x, W.'z, b), 'z) *)
let t_f8 = Term_utility.tapp "f" [t_a; t_W6; t_a]  (* f(a, W.P(a, a, b), a) *)
let t_f9 = Term_utility.tapp "f" [t_a; t_W7; t_Y]  (* f(a, W.P(K.#1, K.#1, b), ?Y) *)
let t_f10 = Term_utility.tapp "f" [t_a; t_W8; t_z]  (* f(a, W.P('x, c, b), 'z) *)
let t_f11 = Term_utility.tapp "f" [t_a; t_W9; t_z]  (* f(a, W.P('x, W.P(?Y, #0, 'z), b), 'z *)

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
  t_a;
  t_b;
  t_c;
  t_0;
  t_1;
  t_x;
  t_z;
  t_Y;
  t_K;
  t_g1;
  t_g2;
  t_P10;
  t_W12;
  t_W13;
  t_Pq;
  t_P1;
  t_P2;
  t_P3;
  t_P4;
  t_P5;
  t_P6;
  t_P7;
  t_P8;
  t_P9;
  t_Wq;
  t_W1;
  t_W2;
  t_W3;
  t_W4;
  t_W5;
  t_W6;
  t_W7;
  t_W8;
  t_W9;
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
  List.nth terms 11;
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
let i_f10 = List.nth indexed_terms 9
let i_f11 = List.nth indexed_terms 10
let query = List.hd terms
let index = Term_index.add_terms Term_index.empty (List.to_seq indexed_terms)
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
