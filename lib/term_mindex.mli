type t

val string_of : ?indent_pattern:string -> ?indent_level:int -> t -> string

val create : unit -> t

val insert_pstring : t -> Pstring.t -> Term.t -> unit

val remove_pstring : t -> Pstring.t -> Term.t -> unit

val insert_term : t -> Term.t -> unit

val remove_term : t -> Term.t -> unit
