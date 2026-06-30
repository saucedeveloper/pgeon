(* Tests that path strings generated from 5 example terms taken from
the Handbook of Automated Reasoning match those from that book *)

(* pgeon/lib $
ocamlc -c utils.mli term.mli term_symbol.mli pstring.mli
*)
(* pgeon/lib $
ocamlc -o pstring_demo.exe utils.ml term.ml term_symbol.ml pstring.ml demo/pstring_demo.ml
*)

open Pgeon

module PstringSetElement = struct
  type t = Pstring.t
  let compare = compare
end

module PstringSet = Set.Make(PstringSetElement)

let _ =
  let run = true in
  if (not run) then () else

  let factory0 = Term.empty_factory in
  let (a, factory1) = Term.create_app "a" [] factory0 in
  let (b, factory2) = Term.create_app "b" [] factory1 in
  let (c, factory3) = Term.create_app "c" [] factory2 in
  let (x, factory4) = Term.create_fvar "*" factory3 in
  let (g1, factory5) = Term.create_app "g" [a; x] factory4 in
  let (g2, factory6) = Term.create_app "g" [x; b] factory5 in
  let (g3, factory7) = Term.create_app "g" [a; b] factory6 in
  let (g4, factory8) = Term.create_app "g" [x; c] factory7 in
  let (f1, factory9) = Term.create_app "f" [g1; c] factory8 in
  let (f2, factory10) = Term.create_app "f" [g2; x] factory9 in
  let (f3, factory11) = Term.create_app "f" [g3; c] factory10 in
  let (f4, factory12) = Term.create_app "f" [g4; b] factory11 in
  let (f5, _factory13) = Term.create_app "f" [x; x] factory12 in
  let terms = [f1; f2; f3; f4; f5] in
  let all_pstrings = Dynarray.create () in
  let print_term_pstrings term =
    Printf.printf "term: %s\n" (Term.string_of term);
    let pstrings = Pstring.all_of_term term in
    Dynarray.append_list all_pstrings pstrings;
    Printf.printf "pstrings: { %s }\n" (
      String.concat ", " (List.map Pstring.string_of pstrings)
    );
    ()
  in
  List.iter print_term_pstrings terms;
  let pstring_set = PstringSet.of_seq (Dynarray.to_seq all_pstrings) in
  let string_of_pstring_set pstring_set =
    let pstring_list = PstringSet.to_list pstring_set in
    let strings = List.map Pstring.string_of pstring_list in
    Printf.sprintf "{ %s }" (String.concat ", " strings)
  in
  Printf.printf "All pstrings: %s\n" (string_of_pstring_set pstring_set);
  ;;
