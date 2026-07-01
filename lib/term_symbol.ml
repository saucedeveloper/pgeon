(* Identifies the term symbol uniquely *)
type t =
| SymBvar of int
| SymFvar
| SymMvar
| SymApp of Term.name
| SymBind of Term.name

let string_of_variant = function
| SymBvar _ -> "Bvar"
| SymFvar -> "Fvar"
| SymMvar -> "Mvar"
| SymApp _ -> "App"
| SymBind _ -> "Bind"

let of_term term =
  match term with
  | Term.Bvar index -> SymBvar index
  | Term.Fvar _ -> SymFvar
  | Term.Mvar _ -> SymMvar
  | Term.App (name, _) -> SymApp name
  | Term.Bind (name, _) -> SymBind name

let string_of (symbol: t) =
  match symbol with
  | SymBvar index -> "#" ^ string_of_int index
  | SymFvar -> "''"
  | SymMvar -> "??"
  | SymApp name -> name
  | SymBind name -> "~" ^ name
