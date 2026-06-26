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

val string_of_weights : variant_weights -> string

val make_variant_weights : float -> float -> float -> float -> float -> variant_weights

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
  -> (Term_symbol.variant * Lfsr_random.t)

val random_variant_template_bank : Lfsr_random.t -> variant_counts -> int -> int
  -> (variant_template_bank * Lfsr_random.t)

val add_random_term : Lfsr_random.t -> generation_options -> variant_template_bank -> Term.factory
  -> (Term.t * Lfsr_random.t * Term.factory)

val add_random_terms :
  Lfsr_random.t -> int -> variant_template_bank -> generation_options -> Term.factory
  -> (TermSet.t * Lfsr_random.t * Term.factory)

val string_of_template_bank : variant_template_bank -> string
