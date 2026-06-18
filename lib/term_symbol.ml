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

let of_term term =
  match term with
  | Factory.Bvar index -> { variant = SymBvar; name = string_of_int index }
  | Factory.Fvar name -> { variant = SymFvar; name = name }
  | Factory.Mvar name -> { variant = SymMvar; name = name }
  | Factory.App (name, _) -> { variant = SymApp; name = name }
  | Factory.Bind (name, _) -> { variant = SymBind; name = name }

let string_of (symbol: t) =
  match symbol.variant with
  | SymBvar -> "#" ^ symbol.name
  | SymFvar -> "'" ^ symbol.name
  | SymMvar -> "?" ^ symbol.name
  | SymApp  -> ""  ^ symbol.name
  | SymBind -> "~" ^ symbol.name
