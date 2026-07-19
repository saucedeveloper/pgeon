(* Type analogue to Term.t designed for easier (but sub-optimal) creation *)
type template =
| TBvar of int
| TFvar of Term.name
| TMvar of Term.name
| TApp  of Term.name * template list
| TBind of Term.name * template

type term_id =
| IBvar of int
| IFvar of Term.name
| IMvar of Term.name
| IApp  of Term.name
| IBind of Term.name

val tbvar : int -> template
val tfvar : string -> template
val tmvar : string -> template
val tapp : string -> template list -> template
val tbind : string -> template -> template
val tconst : string -> template

val create_from_template : template -> Term.factory -> Term.t * Term.factory

val create_many : template Seq.t -> Term.factory -> Term.t list * Term.factory

val string_of_full : ?n:int -> Term.t -> string

val extract_identifier : string -> (term_id * int) option

val string_of_term_id : term_id -> string

val parse : string -> Term.factory -> (Term.t * Term.factory) option
