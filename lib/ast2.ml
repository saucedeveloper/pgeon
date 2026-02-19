type logic_file = {
  types : string list;
  functions : function_decl list;
  binders : binder_decl list;
  rules : rule_decl list;
  strategies : strategy_decl list;
}

and problem_file = { functions : function_decl list; formulas : expr list }
and function_decl = { name : string; params_types : string list; t : string }
and binder_decl = { name : string; variable_type : string; t : string }
and strategy_decl = { name : string; strategy : strategy_kind }

and expr =
  | EVar of string
  | EApp of string * expr list
  | EBind of string * string * expr
  | EBranchTail of string
  | ETreeTail of string

and rule_decl =
  | RuleClosure of { name : string; lhs : expr list }
  | RuleBranch of {
      name : string;
      lhs : expr list;
      rhs : expr list list;
      is_invertible : bool;
      where_clause : where_binding list;
    }
  | RuleTree of {
      name : string;
      lhs : expr list list;
      rhs : expr list list;
      is_invertible : bool;
      where_clause : where_binding list;
    }

and where_binding = { var : string; value : where_expr }
and where_expr = { expr : expr list list; subst : where_subst }

and where_subst =
  | WSubstEntries of where_subst_entry list
  | WSMge of { var1 : string; var2 : string }

and where_subst_entry = { var : string; gen : string }

and strategy_kind =
  | SCall of string
  | SOr of strategy_kind * strategy_kind
  | SThen of strategy_kind * strategy_kind
  | SRepeat of strategy_kind
  | STry of strategy_kind
  | SRuleBang of string
