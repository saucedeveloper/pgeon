(* Type analogue to Term.t designed for easier (but sub-optimal) creation *)
type template =
| TBvar of int
| TFvar of Term.name
| TMvar of Term.name
| TApp  of Term.name * template list
| TBind of Term.name * template

val create_from_template : template -> Term.factory -> Term.t * Term.factory

val create_many : template list -> Term.factory -> Term.t list * Term.factory
