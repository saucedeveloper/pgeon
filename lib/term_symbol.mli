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
  name : Factory.name;
}

val string_of_variant : variant -> string

val of_term : (Factory.term -> t)
