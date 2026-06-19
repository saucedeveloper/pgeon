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

let string_of_variant = function
| SymBvar -> "Bvar"
| SymFvar -> "Fvar"
| SymMvar -> "Mvar"
| SymApp  -> "App"
| SymBind -> "Bind"

let of_term term =
  match term with
  | Term.Bvar index -> { variant = SymBvar; name = string_of_int index }
  | Term.Fvar name -> { variant = SymFvar; name = name }
  | Term.Mvar name -> { variant = SymMvar; name = name }
  | Term.App (name, _) -> { variant = SymApp; name = name }
  | Term.Bind (name, _) -> { variant = SymBind; name = name }

let string_of (symbol: t) =
  match symbol.variant with
  | SymBvar -> "#" ^ symbol.name
  | SymFvar -> "'" ^ symbol.name
  | SymMvar -> "?" ^ symbol.name
  | SymApp  -> ""  ^ symbol.name
  | SymBind -> "~" ^ symbol.name
