type generator =
  Tableau.proof_state -> Term.t -> (Term.t * Tableau.proof_state) option

type unifier = Term.t -> Term.t -> Term.free_substitution option
type t

val create : unit -> t
val register_generator : t -> string -> generator -> unit
val register_unifier : t -> string -> unifier -> unit
val find_generator : t -> string -> generator
val find_unifier : t -> string -> unifier
