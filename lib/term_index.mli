(* Immutable implementation of a term index *)

type t

val string_of : ?indent_pattern:string -> ?indent_level:int -> t -> string

val empty : t

(* Add a pstring that corresponds to a term to the index *)
val add_pstring : t -> Pstring.t -> Factory.term -> t

(* Remove a pstring that corresponds to a term from the index *)
val remove_pstring : t -> Pstring.t -> Factory.term -> t

(* Add all pstrings of a term to the index *)
val add_term : t -> Factory.term -> t

(* Remove all pstrings of a term from the index *)
val remove_term : t -> Factory.term -> t

val get_example_index : Factory.t -> (t * Term.t list * Factory.t)
