(* Mutable implementation of a term index *)
(* Addition and removal must be sound (break no invariant) and
succeed, otherwise they result in an assertion failure *)

module IdComparableTerm : sig
  type t = Term.t
  val equal : t -> t -> bool
  val hash : t -> int
end

(* Set of terms on a leaf of the index *)
module TermSet : sig
  type 'a t = 'a Hashtbl.Make(IdComparableTerm).t
  val mem : 'a t -> Term.t -> bool
  val length : 'a t -> int
end

type term_set = unit TermSet.t

type t

val string_of : ?indent_pattern:string -> ?indent_level:int -> t -> string

val create : ?capacity:int -> unit -> t

val is_empty : t -> bool

(* Insert all path-strings of a term in the index *)
val insert_term : t -> Term.t -> unit

(* Remove all path-strings of a term from the index *)
val remove_term : t -> Term.t -> unit

(* Insert multiple terms to the index *)
val insert_terms : t -> (Term.t Seq.t) -> unit

(* Remove multiple terms from the index *)
val remove_terms : t -> (Term.t Seq.t) -> unit

(* Checks whether this index contains this term *)
val contains_term : t -> Term.t -> bool

(* Checks whether this index contains all of these terms *)
val contains_all_terms : t -> (Term.t Seq.t) -> bool

(* All generalizations of this term contained in the index *)
val retrieve_generalizations : t -> Term.t -> Term.substitutability -> term_set

(* All instances of this term contained in the index *)
val retrieve_instances : t -> Term.t -> Term.substitutability -> term_set

(* All terms unifiable with this term contained in the index *)
val retrieve_unifiable : t -> Term.t -> Term.substitutability -> term_set

(* All variants of this term contained in the index *)
val retrieve_variants : t -> Term.t -> term_set
