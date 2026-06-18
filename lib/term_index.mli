type t

val string_of : t -> string

val empty : t

val add : t -> Pstring.t -> Factory.term -> t

val remove : t -> Pstring.t -> Factory.term -> t

val add_term : t -> Factory.term -> t

val remove_term : t -> Factory.term -> t
