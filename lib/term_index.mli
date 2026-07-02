(* Immutable implementation of a term index *)

module IdComparableTerm : sig
  type t = Term.t
  val compare : t -> t -> int
end

(* Set of terms on a leaf of the index *)
module TermSet : sig
  type t = Set.Make(IdComparableTerm).t
end

type term_set = TermSet.t

val string_of_term_set : ?n:int -> term_set -> string

val string_of_term_set_full : ?n:int -> term_set -> string

type t

type retrieval_options = {
  fvar_instanciable : bool;
  mvar_instanciable : bool;
}

val string_of : ?indent_pattern:string -> ?indent_level:int -> t -> string

val empty : t

val is_empty : t -> bool

(* Add a pstring that corresponds to a term to the index *)
val add_pstring : t -> Pstring.t -> Term.t -> t

(* Remove a pstring that corresponds to a term from the index *)
val remove_pstring : t -> Pstring.t -> Term.t -> t

(* Add all pstrings of a term to the index *)
val add_term : t -> Term.t -> t

(* Remove all pstrings of a term from the index *)
val remove_term : t -> Term.t -> t

val retrieve_generalizations : t -> Term.t -> retrieval_options
  -> term_set

val retrieve_instances : t -> Term.t -> retrieval_options
  -> term_set

val get_example_index : Term.factory -> (t * Term.t list * Term.factory)
