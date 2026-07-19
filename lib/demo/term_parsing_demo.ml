open Pgeon

let () =
  let text = "K.(W(P(f, g), ?b))" in
  let result = Term_utility.extract_identifier text in
  let string_of kvp =
    let (id, len) = kvp in
    Term_utility.string_of_term_id id ^ " (" ^ (string_of_int len) ^ ")"
  in
  Printf.printf "text: %s\n" text;
  Printf.printf "identifier: %s\n" (Utils.debug_string_of_option string_of result);
  let factory = Term.empty_factory in
  let parsed = Term_utility.parse text factory in
  let string_of kvp =
    let (term, _factory) = kvp in
    Term.string_of term
  in
  Printf.printf "parsed: %s\n" (Utils.debug_string_of_option string_of parsed);
  ;;
