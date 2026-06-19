(** Factory: set of `term`s *)
type t

(** Empty factory *)
val empty : t

(** Factory to string recursively *)
val string_of_factory : t -> string

(** Factory to string with partial addresses *)
val debug_string_of_factory : t -> string

val cardinal : t -> int

val create_bvar : int -> t -> (Term.t * t)
val create_fvar : Term.name -> t -> (Term.t * t)
val create_mvar : Term.name -> t -> (Term.t * t)
val create_app : Term.name -> Term.t list -> t -> (Term.t * t)
val create_bind : Term.name -> Term.t -> t -> (Term.t * t)

(** Term to string recursively *)
val string_of_term : (Term.t -> string)

(** Name/identifier of term (not recursive) *)
val identifier_of_term : (Term.t -> string)

(** Term comparison: a - b *)
val term_compare : Term.t -> Term.t -> int

(** Term equality comparison: a = b assuming they are from the same factory *)
val term_equal : Term.t -> Term.t -> bool

val address_of : 'a -> int

val string_address_of : ?n:int -> 'a -> string
