(* Type analogue to Term.t designed for easier (but sub-optimal) creation *)
type template =
| TBvar of int
| TFvar of Term.name
| TMvar of Term.name
| TApp  of Term.name * template list
| TBind of Term.name * template

val tbvar : int -> template
val tfvar : string -> template
val tmvar : string -> template
val tapp : string -> template list -> template
val tbind : string -> template -> template
val tconst : string -> template

val create_from_template : template -> Term.factory -> Term.t * Term.factory

val create_many : template list -> Term.factory -> Term.t list * Term.factory

val string_of_full : ?n:int -> Term.t -> string
