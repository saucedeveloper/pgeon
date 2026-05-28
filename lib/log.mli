type level = Debug | Info | Warn | Error
type formatter = level -> string -> string
type sink = level -> string -> unit

val set_level : level -> unit
val get_level : unit -> level
val set_sink : sink -> unit
val set_formatter : formatter -> unit
val debug : ('a, unit, string, unit) format4 -> 'a
val info : ('a, unit, string, unit) format4 -> 'a
val warn : ('a, unit, string, unit) format4 -> 'a
val error : ('a, unit, string, unit) format4 -> 'a
