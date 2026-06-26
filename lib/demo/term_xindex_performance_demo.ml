(* Compares the performance of term insertion and deletion
in mutable and immutable indexes *)

(* pgeon/lib $
ocamlc -c utils.mli term.mli term_symbol.mli lfsr_random.mli term_generation.mli pstring.mli term_index.mli term_mindex.mli
*)
(* pgeon/lib $
ocamlc -o term_xindex_performance_demo.exe utils.ml term.ml term_symbol.ml lfsr_random.ml term_generation.ml pstring.ml term_index.ml term_mindex.ml demo/term_xindex_performance_demo.ml
*)

module TermSet = Term_generation.TermSet

let index_insert_terms index terms =
  let add_term term current_index =
    let new_index = Term_index.add_term current_index term in
    new_index
  in
  TermSet.fold add_term terms index

let index_remove_terms index terms =
  let remove_term term current_index =
    let new_index = Term_index.remove_term current_index term in
    new_index
  in
  TermSet.fold remove_term terms index

let mindex_insert_terms mindex terms =
  let insert_term term =
    Term_mindex.insert_term mindex term;
    ()
  in
  TermSet.iter insert_term terms

let mindex_remove_terms mindex terms =
  let remove_term term =
    Term_mindex.remove_term mindex term;
    ()
  in
  TermSet.iter remove_term terms

let _ =
  (* let seed = Lfsr_random.choose_seed () in *)
  let seed = Lfsr_random.state_of_int 396897422814058137 in
  Printf.printf "seed = %s\n" (Lfsr_random.string_of_state seed); 
  let term_count = (* 5 *)10000 in
  let name_max_length = 20 in
  let max_arity = 4 in
  let next_state = Lfsr_random.next seed in
  let variant_counts: Term_generation.variant_counts = {
    fvar = 5;
    mvar = 5;
    app  = 5;
    bind = 5;
  } in
  let (template_bank, next_state) = Term_generation.random_variant_template_bank next_state variant_counts name_max_length max_arity in
  Printf.printf "template_bank: %s\n%!" (Term_generation.string_of_template_bank template_bank);

  let weights = Term_generation.make_variant_weights 1.0 1.0 1.0 15.0 10.0 in
  let options: Term_generation.generation_options = {
    max_depth = 4;
    variant_weights = weights;
  } in
  let factory0 = Term.empty_factory in
  let time_start_generation = Sys.time () in
  let (terms, next_state, factory1) = Term_generation.add_random_terms next_state term_count template_bank options factory0 in
  let time_end_generation = Sys.time () in
  Printf.printf "Generation: %fs\n" (time_end_generation -. time_start_generation);
  Printf.printf "Launching with %d/%d wanted terms, factory has %d\n"
    (TermSet.cardinal terms) term_count (Term.factory_cardinal factory1);
  Printf.printf "%s\n" (
    String.concat "\n" (
      List.map Term.string_of (
          List.of_seq (
            Seq.take 10 (TermSet.to_seq terms)
          )
        )
      )
    );
  Printf.printf "...\n%!";
  let time_start_operation = Sys.time () in
  let index = Term_index.empty in
  let mindex = Term_mindex.create () in

  let time_start_imm = Sys.time () in
  let index_inserted = index_insert_terms index terms in
  let time_end_imm = Sys.time () in

  Printf.printf "Immutable insertion: %fs\n%!" (time_end_imm -. time_start_imm);

  let time_start_mut = Sys.time () in
  mindex_insert_terms mindex terms;
  let time_end_mut = Sys.time () in

  Printf.printf "Mutable insertion: %fs\n%!" (time_end_mut -. time_start_mut);

  let time_start_imm = Sys.time () in
  let index_removed = index_remove_terms index_inserted terms in
  let time_end_imm = Sys.time () in

  Printf.printf "Immutable deletion: %fs\n%!" (time_end_imm -. time_start_imm);

  let time_start_mut = Sys.time () in
  mindex_remove_terms mindex terms;
  let time_end_mut = Sys.time () in

  Printf.printf "Mutable deletion: %fs\n%!" (time_end_mut -. time_start_mut);

  let time_end_operation = Sys.time () in
  Printf.printf "\nOperations total: %fs\n%!" (time_end_operation -. time_start_operation);

  let () = if not (Term_index.is_empty index_removed) then (
    Printf.printf "Term index did not end up empty\n";
    Printf.printf "%s\n" (Term_index.string_of index);
    ()
  ) else ()
  in

  let () = if not (Term_mindex.is_empty mindex) then (
    Printf.printf "Term mindex did not end up empty\n";
    Printf.printf "%s\n" (Term_mindex.string_of mindex);
    ()
  ) else ()
  in
  ()
  ;;
