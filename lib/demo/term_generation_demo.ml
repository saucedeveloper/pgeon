(* Tests random (in a controlled way) generation of names and terms *)

(* pgeon/lib $
ocamlc -c utils.mli term.mli term_symbol.mli lfsr_random.mli term_generation.mli
*)
(* pgeon/lib $
ocamlc -o term_generation_demo.exe utils.ml term.ml term_symbol.ml lfsr_random.ml term_generation.ml demo/term_generation_demo.ml
*)

open Pgeon
open Term_generation

let demo_random_letters _ =
  let seed = Lfsr_random.choose_seed () in
  let rand_letter state _i =
    let (letter_index, next_state) = random_letter_index state Term_generation.letter_thresholds in
    let letter = Char.chr (letter_index + Char.code 'a') in
    (next_state, letter)
  in

  let length = 10 in
  let iter_range = List.init length (fun x -> x + 1) in
  let (_next_state, random_string) = List.fold_left_map rand_letter seed iter_range in
  Printf.printf "%s\n" (String.of_seq (List.to_seq random_string));
  ;;

let demo_random_variant_and_name _ =
  let seed = Lfsr_random.choose_seed () in
  let (name, next_state) = random_name seed 10 ~case:CamelCase in
  let weights = make_variant_weights 1.0 1.0 1.0 1.0 1.0 in
  let (variant, _next_state) = random_variant next_state weights in
  Printf.printf "%s %s\n" (Term_symbol.string_of_variant variant) name;
  ;;

let demo_random_term _ =
  (* let seed_with_div0 = 242633790887765721 in *)
  let seed = Lfsr_random.choose_seed () in
  let weights = make_variant_weights 1.0 1.0 1.0 15.0 10.0 in
  Printf.printf "seed: %s, weights: (%s)\n"
    (Lfsr_random.string_of_state seed)
    (Term_generation.string_of_weights weights);
  let name_max_length = 3 in
  let max_arity = 3 in
  let options = {
    max_depth = 4;
    variant_weights = weights;
  } in
  let variant_counts = {
    fvar = 3;
    mvar = 3;
    app  = 3;
    bind = 3;
  } in
  let (template_bank, next_state) = Term_generation.random_variant_template_bank
    seed variant_counts name_max_length max_arity in
  let factory0 = Term.empty_factory in
  let (term, _next_state, _factory1) = Term_generation.add_random_term next_state options template_bank factory0 in
  Printf.printf "%s\n" (Term.string_of term);
  ;;

let demo_multiple_random_names _ =
  for _i = 0 to 10 do
    demo_random_variant_and_name ()
  done
  ;;

let _ =
  Printf.printf "\n----------------------Random-Letters---------------------\n\n";
  demo_random_letters ();
  Printf.printf "\n-----------------Random-Variant-And-Name-----------------\n\n";
  demo_random_variant_and_name ();
  Printf.printf "\n-----------------------Random-Term-----------------------\n\n";
  demo_random_term ();
  ;;
