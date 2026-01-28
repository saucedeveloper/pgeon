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

and tree_expr =
  | TreeLeaf of expr
  | TreeBranch of expr list
  | TreeUnion of tree_expr * tree_expr

and tree_rule = {
  lhs_tree : tree_expr;
  rhs_tree : tree_expr;
  branch_tail : string option;
  tree_var : string;
}

and generator_call = {
  gen_name : string;
  gen_args : expr list;
}

and subst_rhs =
  | SR_Gen of generator_call
  | SR_Expr of where_expr

and subst_entry = {
  target : string;
  rhs : subst_rhs;
}

and where_base =
  | WExpr of expr
  | WTree of tree_expr

and where_subst =
  | SubstEntries of subst_entry list
  | SubstRef of expr

and where_expr = {
  base : where_base;
  substs : where_subst option;
}

and where_binding = {
  var : string;
  value : where_expr;
}

and rule_decl = {
  name : string;
  lhs : expr list;
  arrow : rule_type;
  rhs : expr list list;
  where_clause : where_binding list;
  meta_envs : (string * string list) list;
  tree_rule : tree_rule option;
}

and rule_type = Close | NoInvertible | Invertible

and expr =
  | LVar of string
  | LFun of string * expr list
  | LBinder of string * string * expr

and limit_spec =
  | LimitConst of int
  | LimitDepth

and strategy_decl =
  | Rule of string
  | AndThen of strategy_decl * strategy_decl
  | OrElse of strategy_decl * strategy_decl
  | Repeat of strategy_decl
  | Do of int * strategy_decl
  | Try of strategy_decl
  | Limit of limit_spec * strategy_decl
  | Depth of strategy_decl

val symbol_fvar : t -> string list
val symbol_func : t -> string list
val symbol_bind : t -> string list
val term_of_expr :
  string list -> string list -> string list -> string list -> expr -> Term.t
val is_meta : string -> bool
val compile_strategies :
  t -> (string * Strategy.t) list * Strategy.t
