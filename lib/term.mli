type name = int

type t =
  | Bvar of int
  | Fvar of name
  | Mvar of name
  | App of name * t list
  | Bind of name * t

val equal : t -> t -> bool
val occurs : name -> t -> bool
val var_open : t list -> t -> t
val substitute : (name * t) list -> t -> t
val rule_match : t list -> t list -> (name * t) list option
