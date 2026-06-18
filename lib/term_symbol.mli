(* Identifies the term variant without its contents *)
type term_symbol_variant =
| SymBvar
| SymFvar
| SymMvar
| SymApp
| SymBind

(* Identifies the term symbol uniquely *)
type t = {
  variant : term_symbol_variant;
  name : Factory.name;
}

val of_term : (Factory.term -> t)
