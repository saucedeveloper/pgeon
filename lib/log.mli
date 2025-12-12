type level = Debug | Info | Warn | Error
val int_of_level : level -> int
val string_of_level : level -> string
val min_level : level ref
type formatter = level -> string -> string
type sink = level -> string -> unit
val formatter : formatter ref
val sink : (level -> string -> unit) ref
val set_level : level -> unit
val set_sink : (level -> string -> unit) -> unit
val set_formatter : formatter -> unit
val log : level -> ('a, unit, string, unit) format4 -> 'a
val debug : ('a, unit, string, unit) format4 -> 'a
val info : ('a, unit, string, unit) format4 -> 'a
val warn : ('a, unit, string, unit) format4 -> 'a
val error : ('a, unit, string, unit) format4 -> 'a
