(* Identifies the term symbol uniquely *)
type t =
| SymBvar of int
| SymFvar
| SymMvar
| SymApp of Term.name
| SymBind of Term.name

val string_of_variant : t -> string

val of_term : Term.t -> t

val string_of : t -> string
