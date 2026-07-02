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

type variant_counts = {
  fvar : int;
  mvar : int;
  app  : int;
  bind : int;
}

type generation_options = {
  max_depth : int;
  variant_weights : variant_weights;
}

module Name = struct
  type t = Term.name
  let compare a b = compare a b
end

module NameMap = Map.Make(Name)

(* For each variant (except bvar), contains the map from string to 'a ('a being arity or nothing)
and a copy of that data in the form of an array for constant time access with a random integer.
Possibility to remove maps from the structure as they only provide removal of duplicates during
creation *)
type variant_template_bank = {
  fvar_map : unit NameMap.t;
  fvar_arr : Term.name array;
  mvar_map : unit NameMap.t;
  mvar_arr : Term.name array;
  app_map  : int  NameMap.t;
  app_arr  : (Term.name * int) array;
  bind_map : unit NameMap.t;
  bind_arr : Term.name array;
}

type name_case =
| CamelCase (* eval *)
| UpperCase (* EVAL *)
| PascalCase (* Eval *)

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

let random_pick_array (source: Lfsr_random.t) (array: 'a array) =
  let length = Array.length array in
  let (index, next_state) = Lfsr_random.get_next source length in
  (Array.get array index, next_state)

let random_bvar (source: Lfsr_random.t) (current_depth: int) (factory: Term.factory) =
  assert (0 < current_depth);
  let (value, next_state) = Lfsr_random.get_next source current_depth in
  let (term, new_factory) = Term.create_bvar value factory in
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

let random_name (source: Lfsr_random.t) (name_max_length: int) ~case:case =
  let length = (Lfsr_random.get_current source name_max_length) + 1 in
  let next_state = Lfsr_random.next source in
  let (ends_consonant, next_state) = Lfsr_random.get_next next_state 2 in
  assert (0 < length);
  let iter_range = List.init length (fun x -> x + 1) in
  let base_letter = if (case = UpperCase) then 'A' else 'a' in
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
    let next_state = Lfsr_random.next next_state in
    (letter::letters, next_state)
  in
  let fold_init = ([], next_state) in
  let (all_letters, last_state) = List.fold_left fold_f fold_init iter_range in
  let all_letters_capitalized_maybe =
    if (case <> PascalCase) then
      all_letters
    else (
      match all_letters with
      | [] -> []
      | first::rest -> (Char.Ascii.uppercase first)::rest
    )
  in
  let identifier = String.of_seq (List.to_seq all_letters_capitalized_maybe) in
  (identifier, last_state)

let random_fvar (source: Lfsr_random.t) (bank: Term.name array) (factory: Term.factory) =
  let (name, next_state) = random_pick_array source bank in
  let (term, new_factory) = Term.create_fvar name factory in
  (term, next_state, new_factory)

let random_mvar (source: Lfsr_random.t) (bank: Term.name array) (factory: Term.factory) =
  let (name, next_state) = random_pick_array source bank in
  let (term, new_factory) = Term.create_mvar name factory in
  (term, next_state, new_factory)

(* (arity, state, factory) -> (term list, next_state, next_factory) *)
type make_inner_list_t = int -> Lfsr_random.t -> Term.factory -> (Term.t list * Lfsr_random.t * Term.factory)

let random_app (source: Lfsr_random.t)
               (bank: (Term.name * int) array)
               (make_inner_list: make_inner_list_t)
               (factory: Term.factory) =
  let ((name, arity), next_state) = random_pick_array source bank in
  let (inner_list, next_state, next_factory) = make_inner_list arity next_state factory in
  let (term, next_factory) = Term.create_app name inner_list next_factory in
  (term, next_state, next_factory)

type make_inner_t = Lfsr_random.t -> Term.factory -> (Term.t * Lfsr_random.t * Term.factory)

let random_bind (source: Lfsr_random.t)
                (bank: Term.name array)
                (make_inner: make_inner_t)
                (factory: Term.factory) =
  let (name, next_state) = random_pick_array source bank in
  let capitalized_name = capitalize_name name in
  let (inner, next_state, next_factory) = make_inner next_state factory in
  let (term, next_factory) = Term.create_bind capitalized_name inner next_factory in
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
    (Term.VBvar, next_state)
  else if rolled_number < th.fvar then
    (Term.VFvar, next_state)
  else if rolled_number < th.mvar then
    (Term.VMvar, next_state)
  else if rolled_number < th.app then
    (Term.VApp, next_state)
  else (
    assert (rolled_number <= th.bind);
    (Term.VBind, next_state)
  )

(* Make a random map of (name, arity) key value pairs. Since names are generated pseudo randomly,
collisions are possible. This function tries up to 2 * `count` generations to fill the map with
`count` elements *)
let random_template_bank (source: Lfsr_random.t)
                         (count: int)
                         (name_max_length: int)
                         (generate_value: Lfsr_random.t -> ('a * Lfsr_random.t))
                         ~case:case =
  let add_random_kvp acc =
    let (map, state) = acc in
    let (name, next_state) = random_name state name_max_length ~case:case in
    let (value, next_state) = generate_value next_state in
    let update search =
      match search with
      | None -> Some value
      | Some existing -> Some existing
    in
    let next_map = NameMap.update name update map in
    (next_map, next_state)
  in
  let (app_bank, next_state) = Utils.aggregate_self add_random_kvp (NameMap.empty, source) count in
  if (NameMap.cardinal app_bank) < count then (
    let add_random_kvp_to_fill _i acc =
      let (map, state) = acc in
      let (name, next_state) = random_name state name_max_length ~case:case in
      let (value, next_state) = generate_value next_state in
      let update search =
        match search with
        | None -> Some value
        | Some existing -> Some existing
      in
      let next_map = NameMap.update name update map in
      if (NameMap.cardinal next_map) = count then
        None
      else (
        assert ((NameMap.cardinal next_map) < count);
        Some (next_map, next_state)
      )
    in
    let (filled_app_bank, next_state) = Utils.aggregate_self_until add_random_kvp_to_fill (app_bank, next_state) count in
    (filled_app_bank, next_state)
  ) else
    (app_bank, next_state)

let random_app_template_bank (source: Lfsr_random.t) (count: int) (name_max_length: int) (max_arity: int) =
  let generate_arity state = Lfsr_random.get_next state (max_arity + 1) in
  random_template_bank source count name_max_length generate_arity ~case:CamelCase

let random_name_bank (source: Lfsr_random.t) (count: int) (name_max_length: int) ~case:case =
  let generate_unit state = ((), state) in
  random_template_bank source count name_max_length generate_unit ~case:case

let random_variant_template_bank (source: Lfsr_random.t)
                                 (variant_counts: variant_counts)
                                 (name_max_length: int)
                                 (max_arity: int) =
  let (fvar_bank, next_state) = random_name_bank source variant_counts.fvar name_max_length ~case:CamelCase in
  let (mvar_bank, next_state) = random_name_bank next_state variant_counts.mvar name_max_length ~case:UpperCase in
  let (app_bank, next_state) = random_app_template_bank next_state variant_counts.app name_max_length max_arity in
  let (bind_bank, next_state) = random_name_bank next_state variant_counts.bind name_max_length ~case:PascalCase in

  let fvar_arr = Array.of_seq (Seq.map (fun (name, ()) -> name) (NameMap.to_seq fvar_bank)) in
  let mvar_arr = Array.of_seq (Seq.map (fun (name, ()) -> name) (NameMap.to_seq mvar_bank)) in
  let app_arr  = Array.of_seq (NameMap.to_seq app_bank) in
  let bind_arr = Array.of_seq (Seq.map (fun (name, ()) -> name) (NameMap.to_seq bind_bank)) in

  let template_bank: variant_template_bank = {
    fvar_map = fvar_bank;
    fvar_arr = fvar_arr;
    mvar_map = mvar_bank;
    mvar_arr = mvar_arr;
    app_map  = app_bank;
    app_arr  = app_arr;
    bind_map = bind_bank;
    bind_arr = bind_arr;
  } in
  (template_bank, next_state)

let add_random_term (source: Lfsr_random.t)
                    (options: generation_options)
                    (variant_template_bank: variant_template_bank)
                    (factory: Term.factory) =

  assert (0 < options.max_depth);
  let variant_weights = options.variant_weights in

  let rec recursive (term_depth: int) (bind_depth: int) (state: Lfsr_random.t) (factory: Term.factory) =
    assert (0 <= term_depth);
    assert (term_depth <= options.max_depth);

    let effective_variant_weights: variant_weights = {
      bvar = if (0 < bind_depth) then variant_weights.bvar else 0.0;
      fvar = variant_weights.fvar;
      mvar = variant_weights.mvar;
      app  = if (term_depth < options.max_depth) then variant_weights.app else 0.0;
      bind = if (term_depth < options.max_depth) then variant_weights.bind else 0.0;
    } in
    let (variant, next_state) = random_variant state effective_variant_weights in

    match variant with
    | Term.VBvar -> (random_bvar next_state bind_depth factory)
    | Term.VFvar -> (random_fvar next_state variant_template_bank.fvar_arr factory)
    | Term.VMvar -> (random_mvar next_state variant_template_bank.mvar_arr factory)
    | Term.VApp  -> (
      let make_inner_list arity state factory =
        let iter_range = List.init arity (fun x -> x + 1) in
        let make_inner_term state_factory _i =
          let (next_state, next_factory) = state_factory in
          let (term, next_state, next_factory) = recursive (term_depth + 1) bind_depth next_state next_factory in
          ((next_state, next_factory), term)
        in
        let ((next_state, next_factory), terms) = List.fold_left_map make_inner_term (state, factory) iter_range in
        (terms, next_state, next_factory)
      in
      random_app next_state variant_template_bank.app_arr make_inner_list factory
    )
    | Term.VBind -> (
      let make_inner state factory =
        recursive (term_depth + 1) (bind_depth + 1) state factory
      in
      random_bind next_state variant_template_bank.bind_arr make_inner factory
    )
  in

  let (term, next_state, new_factory):
    Term.t * Lfsr_random.t * Term.factory =
    recursive 1 0 source factory
  in
  (term, next_state, new_factory)

module TermElement = struct
  type t = Term.t
  let compare = Term.term_compare
end

module TermSet = Set.Make(TermElement)

let add_random_terms (source: Lfsr_random.t)
                     (term_count: int)
                     (template_bank: variant_template_bank)
                     (options: generation_options)
                     (factory: Term.factory) =

  let aggregate acc =
    let (terms, state, factory) = acc in
    let (term, next_state, next_factory) = add_random_term state options template_bank factory in
    (TermSet.add term terms, next_state, next_factory)
  in
  let initial = (TermSet.empty, source, factory) in
  let (terms, next_state, next_factory) = Utils.aggregate_self aggregate initial term_count in
  (* assert (term_count = TermSet.cardinal terms); (* very likely to be false *) *)
  (terms, next_state, next_factory)

let string_of_template_bank template_bank =
  let concat_names arr = String.concat ", " (Array.to_list arr) in
  let concat_names_arity arr =
    let kvp_seq = Array.to_seq arr in
    let to_string name_arity =
      let (name, arity) = name_arity in
        if arity = 0 then
          name
        else
          Printf.sprintf "%s(%d)" name arity
    in
    let string_seq = Seq.map to_string kvp_seq in
    String.concat ", " (List.of_seq string_seq)
  in
  Printf.sprintf "{ fvar: { %s }, mvar: { %s }, app: { %s }, bind: { %s } }"
    (concat_names template_bank.fvar_arr)
    (concat_names template_bank.mvar_arr)
    (concat_names_arity template_bank.app_arr)
    (concat_names template_bank.bind_arr)
