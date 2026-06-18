type t

val string_of : ?indent_pattern:string -> ?indent_level:int -> t -> string

val empty : t

(* Add a pstring that corresponds to a term to the index *)
val add : t -> Pstring.t -> Factory.term -> t

(* Remove a pstring that corresponds to a term from the index *)
val remove : t -> Pstring.t -> Factory.term -> t

(* Add all pstrings of a term to the index *)
val add_term : t -> Factory.term -> t

(* Remove all pstrings of a term to the index *)
val remove_term : t -> Factory.term -> t
