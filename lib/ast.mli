type t = {
  types : string list;
  functions : function_decl list;
  binders : binder_decl list;
  rules : rule_decl list;
  strategies : (string * strategy_decl) list;
}

and function_decl = {
  name : string;
  params_types : string list;
  t : string;
}

and binder_decl = { name : string; variable_type : string; t : string }

and rule_decl = {
  name : string;
  lhs : expr list;
  arrow : rule_type;
  rhs : expr list list;
}

and rule_type = Close | NoInvertible | Invertible

and expr =
  | LVar of string
  | LFun of string * expr list
  | LBinder of string * string * expr

and strategy_decl =
  | Rule of string
  | AndThen of strategy_decl * strategy_decl
  | OrElse of strategy_decl * strategy_decl
  | Repeat of strategy_decl
  | Try of strategy_decl

val symbol_fvar : t -> string list
val symbol_func : t -> string list
val symbol_bind : t -> string list
val term_of_expr :
  string list -> string list -> string list -> expr -> Term.t
val compile_strategies :
  t -> (string * Strategy.t) list * Strategy.t
