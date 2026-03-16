type type_decl = string
type functions_decl = { name : string; args : type_decl list; ret : type_decl }
type binders_decl = { name : string; arg : type_decl; ret : type_decl }
type arrow_kind = Close | Invertible | NonInvertible

type expr =
  | EVar of string
  | EApp of string * expr list
  | EBind of string * string * expr

(* (P0 ; P1 ; ...B) is a branch expr, where P0; P1 are expressions and B is the rest of the branch. *)
type branch_expr = expr list * string

(* if ...B is a branch_expr, then ...B | ...T is a tree rule, where ...B is the branch and ...T is the rest of the tree. *)
type tree_expr = branch_expr list * string
type gen_call = { name : string }

type where_op =
  | WhereSubstGen of { bound : string; by : gen_call }
  | WhereUnifier of { name : string; left : expr; right : expr }

type where_clause =
  | WhereExprClause of { dst : string; src : expr; op : where_op }
  | WhereTreeClause of { dst : string; src : tree_expr; op : where_op }

type rule_decl =
  | RuleBranch of {
      name : string;
      arrow : arrow_kind;
      lhs : expr list;
      rhs : expr list list;
      where : where_clause list;
    }
  | RuleTree of {
      name : string;
      arrow : arrow_kind;
      lhs : tree_expr;
      rhs : tree_expr;
      where : where_clause list;
    }

type strategy_expr =
  | SCall of string
  | SBang of string
  | SOrElse of strategy_expr * strategy_expr
  | SAndThen of strategy_expr * strategy_expr
  | SOrAlt of strategy_expr * strategy_expr
  | SAndAlt of strategy_expr * strategy_expr
  | SRepeat of strategy_expr

type strategy_decl = { name : string; body : strategy_expr }

type logic_decl = {
  entry_type : type_decl;
  types : type_decl list;
  functions : functions_decl list;
  binders : binders_decl list;
  rules : rule_decl list;
  entry_strategy : strategy_decl;
  strategies : strategy_decl list;
}

type problem_decl = { functions : functions_decl list; formulas : expr list }
