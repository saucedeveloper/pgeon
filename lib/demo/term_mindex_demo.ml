(* pgeon/lib $ ocamlc utils.ml term.ml term_symbol.ml factory.ml pstring.ml term_mindex.ml demo/term_mindex_demo.ml -o term_mindex_demo.exe *)

let insertion_deletion_demo () =
  let factory0 = Factory.empty in

  let (manual_index, terms, factory1) = Term_mindex.get_example_index factory0 in

  Printf.printf "manual_index: %s\n\n" (Term_mindex.string_of manual_index);

  let procedural_index = Term_mindex.create () in

  (* let print_insertions arg =
    let (pstring, term) = arg in
    Printf.printf "pstring: %s\n" (Pstring.Term_mindex.string_of pstring);
    let () = Term_mindex.insert_pstring procedural_index pstring term in
    Printf.printf "procedural_index: %s\n" (Term_mindex.string_of procedural_index);
    ()
  in *)

  (* let print_insertions_many fterm =
    Printf.printf "\nterm: %s (%s)\n" (Factory.string_of_term fterm) (Factory.string_address_of fterm);
    let pstrings = Pstring.all_of_term fterm in
    let pstrings_with_term = List.map (fun pstr -> (pstr, fterm)) pstrings in
    List.iter print_insertions pstrings_with_term;
    Printf.printf "\n";
  in *)

  let print_insertions_for_term fterm =
    Printf.printf "\nterm: %s (%s)\n" (Factory.string_of_term fterm) (Factory.string_address_of fterm);
    let () = Term_mindex.insert_term procedural_index fterm in
    Printf.printf "procedural_index: %s\n" (Term_mindex.string_of procedural_index);
    ()
  in

  (* let print_deletions arg =
    let (pstring, term) = arg in
    Printf.printf "pstring: %s\n" (Pstring.Term_mindex.string_of pstring);
    let () = Term_mindex.remove_pstring procedural_index pstring term in
    Printf.printf "procedural_index: %s\n" (Term_mindex.string_of procedural_index);
    ()
  in *)

  (* let print_deletions_many fterm =
    Printf.printf "\nterm: %s (%s)\n" (Factory.string_of_term fterm) (Factory.string_address_of fterm);
    let pstrings = Pstring.all_of_term fterm in
    let pstrings_with_term = List.map (fun pstr -> (pstr, fterm)) pstrings in
    List.iter print_deletions pstrings_with_term;
    Printf.printf "\n";
  in *)

  let print_deletions_for_term fterm =
    Printf.printf "\nterm: %s (%s)\n" (Factory.string_of_term fterm) (Factory.string_address_of fterm);
    let () = Term_mindex.remove_term procedural_index fterm in
    Printf.printf "procedural_index: %s\n" (Term_mindex.string_of procedural_index);
    ()
  in

  List.iter print_insertions_for_term terms;
  Printf.printf "\n\nDeletions:\n\n";
  List.iter print_deletions_for_term terms;
  ;;

let _ = insertion_deletion_demo () ;;
