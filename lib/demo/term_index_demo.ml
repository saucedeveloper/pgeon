(* pgeon/lib $ ocamlc utils.ml term.ml factory.ml term_symbol.ml pstring.ml term_index.ml demo/term_index_demo.ml -o term_index_demo.exe *)

type index_fold = {
  index : Term_index.t;
  term : Factory.term;
}

let insertion_deletion_demo () =
  let factory0 = Factory.empty in

  let (manual_index, terms, factory1) = Term_index.get_example_index factory0 in

  Printf.printf "manual_index: %s\n\n" (Term_index.string_of manual_index);

  let procedural_index1 = Term_index.empty in

  (* let print_insertions fold pstring =
    let index = fold.index in
    let term = fold.term in
    Printf.printf "pstring: %s\n" (Pstring.string_of pstring);
    let new_index = Term_index.add_pstring index pstring term in
    Printf.printf "procedural_index: %s\n" (Term_index.string_of new_index);
    { fold with index = new_index }
  in *)

  let print_insertions_many current_index term =
    Printf.printf "\nterm: %s (%s)\n" (Factory.string_of_term term) (Factory.string_address_of term);
    (* let pstrings = Pstring.all_of_term term in *)
    (* let initial = { index = current_index; term = term } in *)
    let new_index = Term_index.add_term current_index term in
    Printf.printf "procedural_index: %s\n" (Term_index.string_of new_index);
    (* let new_index = (List.fold_left print_insertions initial pstrings).index in *)
    Printf.printf "\n";
    new_index
  in

  (* let print_deletions fold pstring =
    let index = fold.index in
    let term = fold.term in
    Printf.printf "pstring: %s\n" (Pstring.string_of pstring);
    let new_index = Term_index.remove_pstring index pstring term in
    Printf.printf "procedural_index: %s\n" (Term_index.string_of new_index);
    { fold with index = new_index }
  in *)

  let print_deletions_many current_index term =
    Printf.printf "\nterm: %s (%s)\n" (Factory.string_of_term term) (Factory.string_address_of term);
    (* let pstrings = Pstring.all_of_term term in
    let initial = { index = current_index; term = term } in *)
    let new_index = Term_index.remove_term current_index term in
    Printf.printf "procedural_index: %s\n" (Term_index.string_of new_index);
    (* let new_index = (List.fold_left print_deletions initial pstrings).index in *)
    Printf.printf "\n";
    new_index
  in

  let procedural_index2 = List.fold_left print_insertions_many procedural_index1 terms in
  Printf.printf "\n\nDeletions:\n\n";
  let _ = List.fold_left print_deletions_many procedural_index2 terms in
  ()
  ;;

let _ = insertion_deletion_demo () ;;
