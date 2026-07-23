(* Random generation of names and terms *)

(* Probability weights distributed among variants *)
type variant_weights = {
  bvar : float;
  fvar : float;
  mvar : float;
  app  : float;
  bind : float;
}

(* Probability thresholds distributed among variants
(the last value represents the scale). A random number
picked uniformly between 0 and scale excluded produces
a custom distribution *)
type variant_thresholds = {
  bvar : int;
  fvar : int;
  mvar : int;
  app  : int;
  bind : int;
}

(* How many names to produce for each variant *)
type variant_counts = {
  fvar : int;
  mvar : int;
  app  : int;
  bind : int;
}

(* Options for random term generation *)
type generation_options = {
  max_depth : int;
  variant_weights : variant_weights;
}

(* (arity, state, factory) -> (term list, next_state, next_factory) *)
type make_inner_list_t = int -> Lfsr_random.t -> Term.factory -> (Term.t list * Lfsr_random.t * Term.factory)

(* (state, factory) -> (term, next_state, next_factory) *)
type make_inner_t = Lfsr_random.t -> Term.factory -> (Term.t * Lfsr_random.t * Term.factory)

module Name : sig
  type t = Term.name
  val compare : Term.name -> Term.name -> int
end

module NameMap : sig
  type 'a t = 'a Map.Make(Name).t
end

(* For each variant (except bvar), contains the map from string to 'a ('a being arity or nothing)
and a copy of that data in the form of an array for constant time access with a random integer *)
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

module TermElement : sig
  type t = Term.t
  val compare : Term.t -> Term.t -> int
end

module TermSet : sig
  type t = Set.Make(TermElement).t
  val cardinal : t -> int
  val fold : (Term.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val iter : (Term.t -> unit) -> t -> unit
  val to_seq : t -> Term.t Seq.t
end

(* Probability thresholds for letters in the latin alphabet distributed
to represent the frequency of letters in english texts *)
val letter_thresholds : int array

(* See `letter_thresholds` for consonants only *)
val consonant_thresholds : int array
(* See `letter_thresholds` for vowels only *)
val vowel_thresholds : int array

(* Get a random index biased by the array of probability thresholds *)
val random_letter_index : Lfsr_random.t -> int array -> (int * Lfsr_random.t)

(* Weights to string *)
val string_of_weights : variant_weights -> string

val make_variant_weights : bvar:float -> fvar:float -> mvar:float -> app:float -> bind:float -> variant_weights

(* Integer thresholds by variant to add bias to a uniform distribution *)
val make_variant_thresholds : variant_weights -> int -> variant_thresholds

val random_bvar : Lfsr_random.t -> int -> Term.factory
  -> (Term.t * Lfsr_random.t * Term.factory)

val random_name : Lfsr_random.t -> int -> case:name_case -> (string * Lfsr_random.t)

val random_fvar : Lfsr_random.t -> Term.name array -> Term.factory
  -> (Term.t * Lfsr_random.t * Term.factory)

val random_mvar : Lfsr_random.t -> Term.name array -> Term.factory
  -> (Term.t * Lfsr_random.t * Term.factory)

val random_app : Lfsr_random.t -> (Term.name * int) array -> make_inner_list_t -> Term.factory
  -> (Term.t * Lfsr_random.t * Term.factory)

val random_bind : Lfsr_random.t -> Term.name array -> make_inner_t -> Term.factory
  -> (Term.t * Lfsr_random.t * Term.factory)

val random_variant : Lfsr_random.t -> variant_weights
  -> (Term.variant * Lfsr_random.t)

val random_variant_template_bank : Lfsr_random.t -> variant_counts -> int -> int
  -> (variant_template_bank * Lfsr_random.t)

(* Add a random term to this factory with options *)
val add_random_term : Lfsr_random.t -> generation_options -> variant_template_bank -> Term.factory
  -> (Term.t * Lfsr_random.t * Term.factory)

(* Add random terms to this factory with options *)
val add_random_terms :
  Lfsr_random.t -> int -> variant_template_bank -> generation_options -> Term.factory
  -> (TermSet.t * Lfsr_random.t * Term.factory)

val string_of_template_bank : variant_template_bank -> string
