
type variant_thresholds = {
  bvar : float;
  fvar : float;
  mvar : float;
  app : float;
}

type generation_options = {
  bvar_max_depth : int;
  name_max_length : int;
  max_arity : int;
  max_depth : int;
  variant_thresholds : variant_thresholds;
}

let make_variant_thresholds bvar fvar mvar app bind =
  assert (0.0 <= bvar);
  assert (0.0 <= fvar);
  assert (0.0 <= mvar);
  assert (0.0 <= app);
  assert (0.0 <= bind);
  let sum = bvar +. fvar +. mvar +. app +. bind in
  {
    bvar = (bvar) /. sum;
    fvar = (bvar +. fvar) /. sum;
    mvar = (bvar +. fvar +. mvar) /. sum;
    app =  (bvar +. fvar +. mvar +. app) /. sum;
  }

let random_bvar (source: Lfsr_random.t) (current_depth: int) (factory: Factory.t) =
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
    let offset = if ((i mod 2) = 0) <> (ends_consonant = 0) then
      get_consonant_index (Lfsr_random.get_current state 20)
    else
      get_vowel_index (Lfsr_random.get_current state 6)
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

let random_app (source: Lfsr_random.t) (max_length: int) (make_inner_list: Lfsr_random.t -> (Term.t list * Lfsr_random.t)) (factory: Factory.t) =
  let (name, next_state) = random_name source max_length ~uppercase:false in
  let (inner_list, next_state) = make_inner_list next_state in
  let (term, new_factory) = Factory.create_app name inner_list factory in
  (term, next_state, new_factory)

let random_bind (source: Lfsr_random.t) (max_length: int) (make_inner: Lfsr_random.t -> (Term.t * Lfsr_random.t)) (factory: Factory.t) =
  let (name, next_state) = random_name source max_length ~uppercase:false in
  let capitalized_name = Printf.sprintf "%c%s"
    (Char.uppercase_ascii (String.get name 0))
    (String.sub name 1 ((String.length name) - 1))
  in
  let (inner, next_state) = make_inner next_state in
  let (term, new_factory) = Factory.create_bind capitalized_name inner factory in
  (term, next_state, new_factory)

let random_variant (source: Lfsr_random.t) (thresholds: variant_thresholds) =
  let scale = 0x10000 in
  let bvar_int_th = int_of_float (Float.round (thresholds.bvar *. (float_of_int scale))) in
  let fvar_int_th = int_of_float (Float.round (thresholds.fvar *. (float_of_int scale))) in
  let mvar_int_th = int_of_float (Float.round (thresholds.mvar *. (float_of_int scale))) in
  let app_int_th  = int_of_float (Float.round (thresholds.app  *. (float_of_int scale))) in
  let (rolled_number, next_state) = Lfsr_random.get_next source scale in
  if rolled_number < bvar_int_th then
    (Term_symbol.SymBvar, next_state)
  else if rolled_number < fvar_int_th then
    (Term_symbol.SymFvar, next_state)
  else if rolled_number < mvar_int_th then
    (Term_symbol.SymMvar, next_state)
  else if rolled_number < app_int_th then
    (Term_symbol.SymApp, next_state)
  else
    (Term_symbol.SymBind, next_state)

let _ =
  let seed = Lfsr_random.choose_seed () in
  let (name, next_state) = random_name seed 8 ~uppercase:false in
  let thresholds = make_variant_thresholds 0.1 0.1 1.0 0.1 0.1 in
  let (variant, next_state) = random_variant next_state thresholds in
  Printf.printf "%s %s\n" (Term_symbol.string_of_variant variant) name;
  ;;
