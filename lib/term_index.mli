(* Immutable implementation of a term index *)

type t

val string_of : ?indent_pattern:string -> ?indent_level:int -> t -> string

val empty : t

(* Add a pstring that corresponds to a term to the index *)
val add_pstring : t -> Pstring.t -> Term.t -> t

(* Remove a pstring that corresponds to a term from the index *)
val remove_pstring : t -> Pstring.t -> Term.t -> t

(* Add all pstrings of a term to the index *)
val add_term : t -> Term.t -> t

(* Remove all pstrings of a term from the index *)
val remove_term : t -> Term.t -> t

val get_example_index : Term.factory -> (t * Term.t list * Term.factory)
