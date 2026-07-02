open Pgeon

module PstringSetElement = struct
  type t = Pstring.t
  let compare = compare
end

module PstringSet = Set.Make(PstringSetElement)

let factory0 = Term.empty_factory

let t_bvar = Term_utility.tbvar 0
let t_fvar = Term_utility.tfvar "x"
let t_mvar = Term_utility.tmvar "Y"
let t_const = Term_utility.tconst "a"
let t_appf = Term_utility.tapp "f" [t_bvar; t_const; t_fvar]
let t_appg = Term_utility.tapp "g" [t_mvar; t_appf]
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

let (terms, factory1) = Term_utility.create_many templates factory0

(* W.g(?Y, f(#0, a, 'x)) *)
let total_term = List.hd terms
let pstrings = Pstring.all_of_term total_term
let pstring_set = PstringSet.of_list pstrings

(* pstrings contain no duplicates *)
let%test _ = (List.length pstrings) = (PstringSet.cardinal pstring_set)

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

(* pstrings contains W.0.g.0.?? *)
let%test _ = PstringSet.mem expected_pstring0 pstring_set

(* pstrings contains W.0.g.1.f.0.#0 *)
let%test _ = PstringSet.mem expected_pstring1 pstring_set

(* pstrings contains W.0.g.1.f.1.a *)
let%test _ = PstringSet.mem expected_pstring2 pstring_set

(* pstrings contains W.0.g.1.f.1.'' *)
let%test _ = PstringSet.mem expected_pstring3 pstring_set

(* pstrings contains only those expected *)
let%test _ = PstringSet.cardinal pstring_set = 4
