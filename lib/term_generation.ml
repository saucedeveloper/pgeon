(* pgeon/lib $ ocamlc utils.ml term.ml term_symbol.ml factory.ml lfsr_random.ml term_generation.ml -o term_generation.exe *)

type variant_weights = {
  bvar : float;
  fvar : float;
  mvar : float;
  app  : float;
  bind : float;
}

type variant_thresholds = {
  bvar : int;
  fvar : int;
  mvar : int;
  app  : int;
  bind : int;
}

type generation_options = {
  name_max_length : int;
  max_arity : int;
  max_depth : int;
  variant_weights : variant_weights;
}

let letter_thresholds = [|
  8187;
  9685;
  12481;
  16774;
  29455;
  31651;
  33648;
  39739;
  46728;
  46888;
  47657;
  51650;
  54047;
  60736;
  68225;
  70122;
  70242;
  76233;
  82523;
  91609;
  94405;
  95383;
  97779;
  97929;
  99926;
  100000;
|]

let consonant_thresholds = [|
  2502;
  7172;
  14344;
  18014;
  21350;
  31524;
  31791;
  33075;
  39747;
  43750;
  54925;
  58095;
  58295;
  68302;
  78810;
  93989;
  95623;
  99626;
  99877;
  100000;
|]

let vowel_thresholds = [|20398; 51990; 69403; 88060; 95025; 100000|]

let binary_search_upper (compare: 'a -> 'a -> int) (array: 'a array) (target: 'a) =
  let rec recursive lower upper =
    let width = upper - lower in
    assert (0 < width);
    if width = 1 then
      lower
    else
      let mid = (lower + upper) / 2 in
      let mid_value = array.(mid) in
      let comparison = compare target mid_value in
      if comparison = 0 then
        mid
      else if comparison < 0 then
        recursive lower mid
      else
        recursive mid upper
  in
  recursive 0 (Array.length array)

let random_letter_index (state: Lfsr_random.t) (thresholds: int array) =
  let scale = thresholds.((Array.length thresholds) - 1) in
  let (rolled_number, next_state) = Lfsr_random.get_next state scale in
  let letter_index = binary_search_upper compare thresholds rolled_number in
  (letter_index, next_state)

let string_of_weights (we: variant_weights) =
  Printf.sprintf "bvar: %f, fvar: %f, mvar: %f, app: %f, bind: %f"
  we.bvar we.fvar we.mvar we.app we.bind

let make_variant_weights bvar fvar mvar app bind =
  assert (0.0 <= bvar);
  assert (0.0 <= fvar);
  assert (0.0 <= mvar);
  assert (0.0 <= app);
  assert (0.0 <= bind);
  let result: variant_weights = {
    bvar = bvar;
    fvar = fvar;
    mvar = mvar;
    app =  app;
    bind = bind;
  } in
  result

let make_variant_thresholds (weights: variant_weights) (scale: int) =
  let (bvar, fvar, mvar, app, bind) =
    (weights.bvar, weights.fvar, weights.mvar, weights.app, weights.bind)
  in
  let sum = bvar +. fvar +. mvar +. app +. bind in
  let factor = (float_of_int scale) /. sum in
  let result: variant_thresholds = {
    bvar = int_of_float (Float.round ((bvar) *. factor));
    fvar = int_of_float (Float.round ((bvar +. fvar) *. factor));
    mvar = int_of_float (Float.round ((bvar +. fvar +. mvar) *. factor));
    app =  int_of_float (Float.round ((bvar +. fvar +. mvar +. app) *. factor));
    bind = scale;
  } in
  result

let random_bvar (source: Lfsr_random.t) (current_depth: int) (factory: Factory.t) =
  assert (0 < current_depth);
  let value = Lfsr_random.get_current source current_depth in
  let next_state = Lfsr_random.next source in
  let (term, new_factory) = Factory.create_bvar value factory in
  (term, next_state, new_factory)

let vowel_indices = [0; 4; 8; 14; 20; 24]

let get_vowel_index i = List.nth vowel_indices i

let get_consonant_index i =
  assert (0 <= i);
  assert (i < (26 - (List.length vowel_indices)));
  let rec recursive rem_vowels eq_i =
    match rem_vowels with
    | [] -> eq_i
    | vowel::new_rem_vowels -> (
      if eq_i < vowel then
        eq_i
      else
        recursive new_rem_vowels (eq_i + 1)
    )
  in
  recursive vowel_indices i

let capitalize_name name =
  Printf.sprintf "%c%s"
  (Char.uppercase_ascii (String.get name 0))
  (String.sub name 1 ((String.length name) - 1))

let random_name (source: Lfsr_random.t) (name_max_length: int) ~uppercase:uppercase =
  let length = (Lfsr_random.get_current source (name_max_length - 1)) + 1 in
  let next_state = Lfsr_random.next source in
  let (ends_consonant, next_state) = Lfsr_random.get_next next_state 2 in
  assert (0 < length);
  let iter_range = List.init length (fun x -> x + 1) in
  let base_letter = if uppercase then 'A' else 'a' in
  let ascii_start = Char.code base_letter in
  let fold_f letters_and_state i =
    let (letters, state) = letters_and_state in
    let (offset, next_state) = if ((i mod 2) = 0) <> (ends_consonant = 0) then
      let (consonant_index, next_state) = random_letter_index state consonant_thresholds in
      (get_consonant_index consonant_index, next_state)
    else
      let (vowel_index, next_state) = random_letter_index state vowel_thresholds in
      (get_vowel_index vowel_index, next_state)
    in
    let letter = Char.chr (offset + ascii_start) in
    let next_state = Lfsr_random.next state in
    (letter::letters, next_state)
  in
  let fold_init = ([], next_state) in
  let (all_letters, last_state) = List.fold_left fold_f fold_init iter_range in
  let identifier = String.of_seq (List.to_seq all_letters) in
  (identifier, last_state)

let random_fvar (source: Lfsr_random.t) (max_length: int) (factory: Factory.t) =
  let (name, next_state) = random_name source max_length ~uppercase:false in
  let (term, new_factory) = Factory.create_fvar name factory in
  (term, next_state, new_factory)

let random_mvar (source: Lfsr_random.t) (max_length: int) (factory: Factory.t) =
  let (name, next_state) = random_name source max_length ~uppercase:true in
  let (term, new_factory) = Factory.create_mvar name factory in
  (term, next_state, new_factory)

(* (arity, state, factory) -> (term list, next_state, next_factory) *)
type make_inner_list_t = int -> Lfsr_random.t -> Factory.t -> (Term.t list * Lfsr_random.t * Factory.t)

let random_app (source: Lfsr_random.t)
               (max_length: int)
               (max_arity: int)
               (make_inner_list: make_inner_list_t)
               (factory: Factory.t) =
  let (name, next_state) = random_name source max_length ~uppercase:false in
  let (arity, next_state) = Lfsr_random.get_next next_state max_arity in
  let (inner_list, next_state, next_factory) = make_inner_list arity next_state factory in
  let (term, next_factory) = Factory.create_app name inner_list next_factory in
  (term, next_state, next_factory)

let random_bind (source: Lfsr_random.t) (max_length: int) (make_inner: Lfsr_random.t -> Factory.t -> (Term.t * Lfsr_random.t * Factory.t)) (factory: Factory.t) =
  let (name, next_state) = random_name source max_length ~uppercase:false in
  let capitalized_name = capitalize_name name in
  let (inner, next_state, next_factory) = make_inner next_state factory in
  let (term, next_factory) = Factory.create_bind capitalized_name inner next_factory in
  (term, next_state, next_factory)

let random_variant (source: Lfsr_random.t) (weights: variant_weights) =
  let scale = 100000 in
  let th = make_variant_thresholds weights scale in
  
  assert (0 <= th.bvar && th.bvar <= scale);
  assert (0 <= th.fvar && th.fvar <= scale);
  assert (0 <= th.mvar && th.mvar <= scale);
  assert (0 <= th.app && th.app <= scale);
  assert (th.bvar <= th.fvar);
  assert (th.fvar <= th.mvar);
  assert (th.mvar <= th.app);
  assert (th.app <= th.bind);

  let (rolled_number, next_state) = Lfsr_random.get_next source scale in
  if rolled_number < th.bvar then
    (Term_symbol.SymBvar, next_state)
  else if rolled_number < th.fvar then
    (Term_symbol.SymFvar, next_state)
  else if rolled_number < th.mvar then
    (Term_symbol.SymMvar, next_state)
  else if rolled_number < th.app then
    (Term_symbol.SymApp, next_state)
  else (
    assert (rolled_number <= th.bind);
    (Term_symbol.SymBind, next_state)
  )

let add_random_term (source: Lfsr_random.t) (options: generation_options) (factory: Factory.t) =
  assert (0 < options.max_depth);
  let rec recursive (term_depth: int) (bind_depth: int) (state: Lfsr_random.t) (factory: Factory.t) =
    assert (0 <= term_depth);
    assert (term_depth <= options.max_depth);

    let variant_weights: variant_weights = {
      bvar = if (0 < bind_depth) then options.variant_weights.bvar else 0.0;
      fvar = options.variant_weights.fvar;
      mvar = options.variant_weights.mvar;
      app  = if (term_depth < options.max_depth) then options.variant_weights.app else 0.0;
      bind = if (term_depth < options.max_depth) then options.variant_weights.bind else 0.0;
    } in
    let (variant, next_state) = random_variant state variant_weights in

    match variant with
    | Term_symbol.SymBvar -> (random_bvar next_state bind_depth factory)
    | Term_symbol.SymFvar -> (random_fvar next_state options.name_max_length factory)
    | Term_symbol.SymMvar -> (random_mvar next_state options.name_max_length factory)
    | Term_symbol.SymApp  -> (
      let make_inner_list arity state factory =
        let iter_range = List.init arity (fun x -> x + 1) in
        let make_inner_term state_factory _i =
          let (next_state, next_factory) = state_factory in
          let (term, next_state, next_factory) = recursive (term_depth + 1) bind_depth next_state next_factory in
          ((next_state, next_factory), term)
        in
        let ((next_state, next_factory), terms) = List.fold_left_map make_inner_term (next_state, factory) iter_range in
        (terms, next_state, next_factory)
      in
      random_app next_state options.name_max_length options.max_arity make_inner_list factory
    )
    | Term_symbol.SymBind -> (
      let make_inner state factory =
        recursive (term_depth + 1) (bind_depth + 1) state factory
      in
      random_bind next_state options.name_max_length make_inner factory
    )
  in

  let (term, next_state, new_factory):
    Term.t * Lfsr_random.t * Factory.t =
    recursive 1 0 source factory
  in
  (term, next_state, new_factory)

let add_random_terms (source: Lfsr_random.t)
                     (count: int)
                     (options: generation_options)
                     (factory: Factory.t) =
  let aggregate acc =
    let (state, factory) = acc in
    let (_term, next_state, next_factory) = add_random_term state options factory in
    (next_state, next_factory)
  in
  let (next_state, next_factory) = Utils.aggregate_self aggregate (source, factory) count in
  (next_state, next_factory)

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
