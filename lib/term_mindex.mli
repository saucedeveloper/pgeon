(* Mutable implementation of a term index *)

type t

val string_of : ?indent_pattern:string -> ?indent_level:int -> t -> string

val create : ?capacity:int -> unit -> t

val is_empty : t -> bool

(* Insert a pstring that corresponds to a term in the index *)
val insert_pstring : t -> Pstring.t -> Term.t -> unit

(* Remove a pstring that corresponds to a term from the index *)
val remove_pstring : t -> Pstring.t -> Term.t -> unit

(* Insert all pstrings of a term in the index *)
val insert_term : t -> Term.t -> unit

(* Remove all pstrings of a term from the index *)
val remove_term : t -> Term.t -> unit

val get_example_index : Term.factory -> (t * Term.t list * Term.factory)
