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

let expected_pstring0 = Pstring.( Term_symbol.[|
  { index = node_root_index; symbol = SymBind "W" };
  { index = 0; symbol = SymApp "g" };
  { index = 0; symbol = SymMvar };
|] )

let expected_pstring1 = Pstring.( Term_symbol.[|
  { index = node_root_index; symbol = SymBind "W" };
  { index = 0; symbol = SymApp "g" };
  { index = 1; symbol = SymApp "f" };
  { index = 0; symbol = SymBvar 0 };
|] )

let expected_pstring2 = Pstring.( Term_symbol.[|
  { index = node_root_index; symbol = SymBind "W" };
  { index = 0; symbol = SymApp "g" };
  { index = 1; symbol = SymApp "f" };
  { index = 1; symbol = SymApp "a" };
|] )

let expected_pstring3 = Pstring.( Term_symbol.[|
  { index = node_root_index; symbol = SymBind "W" };
  { index = 0; symbol = SymApp "g" };
  { index = 1; symbol = SymApp "f" };
  { index = 2; symbol = SymFvar };
|] )

(* index contains W.0.g.0.?? *)
let%test _ = Option.is_some (Term_index.find_pstring index_with_terms expected_pstring0)

(* index contains W.0.g.1.f.0.#0 *)
let%test _ = Option.is_some (Term_index.find_pstring index_with_terms expected_pstring1)

(* index contains W.0.g.1.f.1.a *)
let%test _ = Option.is_some (Term_index.find_pstring index_with_terms expected_pstring2)

(* index contains W.0.g.1.f.1.'' *)
let%test _ = Option.is_some (Term_index.find_pstring index_with_terms expected_pstring3)

(* index contains W.0.g.0.?? *)
let%test _ = Option.is_none (Term_index.find_pstring index_with_terms_removed expected_pstring0)

(* index contains W.0.g.1.f.0.#0 *)
let%test _ = Option.is_none (Term_index.find_pstring index_with_terms_removed expected_pstring1)

(* index contains W.0.g.1.f.1.a *)
let%test _ = Option.is_none (Term_index.find_pstring index_with_terms_removed expected_pstring2)

(* index contains W.0.g.1.f.1.'' *)
let%test _ = Option.is_none (Term_index.find_pstring index_with_terms_removed expected_pstring3)

(* index is empty *)
let%test _ = Term_index.is_empty index_with_terms_removed
