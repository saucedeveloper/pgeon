type name = int

type t =
  | Bvar of int
  | Fvar of name
  | Mvar of name
  | App of name * t list
  | Bind of name * t

type substitution_map = (name * t) list

type substitution =
  | MetaSubstitution of substitution_map
  | FreeVarSubstitution of substitution_map

val occurs : name -> t -> bool
val equal : t -> t -> bool
val var_open : t list -> t -> t
val substitute : substitution -> t -> t
val match_rule : t list -> t list -> (int list * substitution) Seq.t
val unify : t list -> t list -> substitution option
val meta_substitution_add : substitution -> (name * t) -> substitution
val merge_meta_substitutions :
  substitution -> substitution -> substitution option
val free_substitution_add : substitution -> (name * t) -> substitution
val merge_free_substitutions :
  substitution -> substitution -> substitution option
val is_empty_substitution : substitution -> bool
