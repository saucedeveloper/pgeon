(* Identifies the term variant without its contents *)
type variant =
| SymBvar
| SymFvar
| SymMvar
| SymApp
| SymBind

(* Identifies the term symbol uniquely *)
type t = {
  variant : variant;
  name : Term.name;
}

val string_of_variant : variant -> string

val of_term : Term.t -> t

val string_of : t -> string
