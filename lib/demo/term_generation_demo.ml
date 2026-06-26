(* pgeon/lib $ ocamlc utils.ml term.ml term_symbol.ml factory.ml lfsr_random.ml term_generation.ml demo/term_generation_demo.ml -o term_generation_dmeo.exe *)

open Term_generation

let demo_random_variant_and_name _ =
  let seed = Lfsr_random.choose_seed () in
  let (name, next_state) = random_name seed 10 ~uppercase:false in
  let weights = make_variant_weights 1.0 1.0 1.0 1.0 1.0 in
  let (variant, next_state) = random_variant next_state weights in
  Printf.printf "%s %s\n" (Term_symbol.string_of_variant variant) name;
  ;;

let demo_random_term _ =
  (* let seed_with_div0 = 242633790887765721 in *)
  let seed = Lfsr_random.choose_seed () in
  let weights = make_variant_weights 1.0 1.0 1.0 15.0 10.0 in
  Printf.printf "seed: %s, weights: (%s)\n" (Lfsr_random.string_of_state seed) (string_of_weights weights);
  let options = {
    name_max_length = 3;
    max_arity = 3;
    max_depth = 4;
    variant_weights = weights;
  } in
  let factory0 = Factory.empty in
  let (term, next_state, factory1) = add_random_term seed options factory0 in
  Printf.printf "%s\n" (Factory.string_of_term term);
  ;;

let demo_random_letter _ =
  let seed = Lfsr_random.choose_seed () in
  let rand_letter state _i =
    let (letter_index, next_state) = random_letter_index state letter_thresholds in
    let letter = Char.chr (letter_index + Char.code 'a') in
    (next_state, letter)
  in

  let length = 10 in
  let iter_range = List.init length (fun x -> x + 1) in
  let (next_state, random_string) = List.fold_left_map rand_letter seed iter_range in
  Printf.printf "%s\n" (String.of_seq (List.to_seq random_string));
  ;;

let demo_multiple_random_names _ =
  for i = 0 to 10 do
    demo_random_variant_and_name ()
  done
  ;;

let _ = demo_random_term ();;
