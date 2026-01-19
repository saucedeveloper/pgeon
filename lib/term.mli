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
val subst_bvar : t -> int -> t -> t
val subst_fvar : (name * t) list -> t -> t
val compose_fvar_subst :
  (name * t) list -> (name * t) list -> (name * t) list
type 'a generator = unit -> 'a option
val rule_match : t list -> t list -> (name * t) list option
val rule_match_gen : t list -> t list -> (name * t) list generator
val unify : t -> t -> (name * t) list option
