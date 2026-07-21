(* Immutable implementation of a term index *)
(* Addition and removal must be sound (break no invariant) and
succeed, otherwise they result in an assertion failure *)

module IdComparableTerm : sig
  type t = Term.t
  val compare : t -> t -> int
end

(* Set of terms on a leaf of the index *)
module TermSet : sig
  type t = Set.Make(IdComparableTerm).t
  val mem : Term.t -> t -> bool
  val subset : t -> t -> bool
  val cardinal : t -> int
end

type term_set = TermSet.t

val string_of_term_set : ?n:int -> term_set -> string

val string_of_term_set_full : ?n:int -> ?sep:string -> term_set -> string

type t

val string_of : ?indent_pattern:string -> ?indent_level:int -> t -> string

val empty : t

val is_empty : t -> bool

(* Add all path-strings of a term to the index *)
val add_term : t -> Term.t -> t

(* Remove all path-strings of a term from the index *)
val remove_term : t -> Term.t -> t

(* Add multiple terms to the index *)
val add_terms : t -> Term.t Seq.t -> t

(* Remove multiple terms from the index *)
val remove_terms : t -> Term.t Seq.t -> t

val contains_term : t -> Term.t -> bool

(* All generalizations of this term contained in the index *)
val retrieve_generalizations : t -> Term.t -> Term.substitutability -> term_set

(* All instances of this term contained in the index *)
val retrieve_instances : t -> Term.t -> Term.substitutability -> term_set

(* All terms unifiable with this term contained in the index *)
val retrieve_unifiable : t -> Term.t -> Term.substitutability -> term_set

(* All variants of this term contained in the index *)
val retrieve_variants : t -> Term.t -> term_set
