(* Mutable implementation of a term index *)

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
