type t

val choose_seed : unit -> t

val next : t -> t

val get_current : t -> int

val get_current_raw : t -> int

val get_current_raw_positive : t -> int

val get_next : t -> (int * t)

val string_of_state : t -> string

val state_of_int : int -> t
