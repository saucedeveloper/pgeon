%{
open Ast

module StringOrd = struct
  type t = string
  let compare = String.compare
end

module StringSet = Set.Make (StringOrd)
module StringMap = Map.Make (StringOrd)

type where_map = Ast.where_expr StringMap.t

let empty_acc = {
  types = [];
  functions = [];
  binders = [];
  rules = [];
  strategies = [];
}

let empty_where : Ast.where_binding list = []

type lhs_info = {
  deps_map : StringSet.t StringMap.t;
  env_map : string list StringMap.t;
  binders : StringSet.t;
}

let set_of_env env =
  List.fold_left (fun acc name -> StringSet.add name acc) StringSet.empty env

let collect_lhs lhs =
  let rec collect_expr env info = function
    | LVar v when Ast.is_meta v ->
        let deps =
          match StringMap.find_opt v info.deps_map with
          | Some s -> s
          | None -> StringSet.empty
        in
        let deps = StringSet.union deps (set_of_env env) in
        let env_map =
          match StringMap.find_opt v info.env_map with
          | None -> StringMap.add v env info.env_map
          | Some existing ->
              if existing <> env then
                failwith
                  (Printf.sprintf
                     "Inconsistent binder environment for meta variable %s" v);
              info.env_map
        in
        {
          info with
          deps_map = StringMap.add v deps info.deps_map;
          env_map;
        }
    | LVar _ -> info
    | LFun (_, args) ->
        List.fold_left (collect_expr env) info args
    | LBinder (_, v, body) ->
        let info = { info with binders = StringSet.add v info.binders } in
        collect_expr (v :: env) info body
  in
  List.fold_left
    (collect_expr [])
    { deps_map = StringMap.empty; env_map = StringMap.empty; binders = StringSet.empty }
    lhs

type rule_ctx = {
  deps_map : StringSet.t StringMap.t;
  where_defs : where_map;
  lhs_binders : StringSet.t;
}

let rec free_binders_expr ctx env visited = function
  | LVar v ->
      if StringMap.mem v ctx.where_defs then
        if StringSet.mem v visited then
          StringSet.empty
        else
          let visited' = StringSet.add v visited in
          free_binders_wexpr ctx env visited'
            (StringMap.find v ctx.where_defs)
      else if Ast.is_meta v then
        let deps =
          match StringMap.find_opt v ctx.deps_map with
          | Some s -> s
          | None -> StringSet.empty
        in
        StringSet.diff deps env
      else if StringSet.mem v ctx.lhs_binders then
        if StringSet.mem v env then StringSet.empty else StringSet.singleton v
      else
        StringSet.empty
  | LFun (_, args) ->
      List.fold_left
        (fun acc expr ->
          StringSet.union acc (free_binders_expr ctx env visited expr))
        StringSet.empty args
  | LBinder (_, v, body) ->
      free_binders_expr ctx (StringSet.add v env) visited body

and free_binders_tree ctx env visited = function
  | Ast.TreeLeaf e -> free_binders_expr ctx env visited e
  | Ast.TreeBranch el ->
      List.fold_left
        (fun acc expr ->
          StringSet.union acc (free_binders_expr ctx env visited expr))
        StringSet.empty el
  | Ast.TreeUnion (a, b) ->
      let acc = free_binders_tree ctx env visited a in
      StringSet.union acc (free_binders_tree ctx env visited b)

and free_binders_wexpr ctx env visited (wexpr : Ast.where_expr) =
  let base =
    match wexpr.base with
    | Ast.WExpr e -> free_binders_expr ctx env visited e
    | Ast.WTree tree -> free_binders_tree ctx env visited tree
  in
  match wexpr.substs with
  | None -> base
  | Some (Ast.SubstRef expr) ->
      let deps = free_binders_expr ctx env visited expr in
      StringSet.union base deps
  | Some (Ast.SubstEntries entries) ->
      List.fold_left
        (fun acc subst ->
          let acc = StringSet.remove subst.target acc in
          match subst.rhs with
          | SR_Gen call ->
              let _ = call.gen_name in
              let deps =
                List.fold_left
                  (fun deps expr ->
                    StringSet.union deps
                      (free_binders_expr ctx env visited expr))
                  StringSet.empty call.gen_args
              in
              StringSet.union acc deps
          | SR_Expr expr ->
              let deps = free_binders_wexpr ctx env visited expr in
              StringSet.union acc deps)
        base entries

let bindings_to_map rule_name clause =
  List.fold_left
    (fun map (binding : Ast.where_binding) ->
      if StringMap.mem binding.var map then
        failwith
          (Printf.sprintf "Duplicate where binding for %s in rule %s"
             binding.var rule_name);
      StringMap.add binding.var binding.value map)
    StringMap.empty clause

let check_rule rule_name (lhs_info : lhs_info) rhs where_clause =
  let where_defs = bindings_to_map rule_name where_clause in
  let ctx =
    {
      deps_map = lhs_info.deps_map;
      where_defs;
      lhs_binders = lhs_info.binders;
    }
  in
  let free_in_rhs =
    List.fold_left
      (fun acc branch ->
        List.fold_left
      (fun acc expr ->
        StringSet.union acc
          (free_binders_expr ctx StringSet.empty StringSet.empty expr))
      acc branch)
    StringSet.empty rhs
  in
  let offending = StringSet.inter free_in_rhs ctx.lhs_binders in
  if not (StringSet.is_empty offending) then
    let vars = String.concat ", " (StringSet.elements offending) in
    failwith
      (Printf.sprintf
         "Rule %s: missing generator for bound variable(s): %s" rule_name vars)
  else ()

let ensure_unique_bindings clause =
  let rec aux set = function
    | [] -> []
    | (binding : Ast.where_binding) :: tl ->
        if StringSet.mem binding.var set then
          failwith
            (Printf.sprintf "Duplicate where binding for %s" binding.var)
        else
          binding :: aux (StringSet.add binding.var set) tl
  in
  aux StringSet.empty clause

let extend_env_map env_map clause =
  List.fold_left
    (fun map (binding : Ast.where_binding) ->
      let value = binding.value in
      let new_env =
        match value.base with
        | Ast.WExpr (LVar name) when Ast.is_meta name -> (
            match value.substs with
            | Some (Ast.SubstEntries entries) ->
                let base_env =
                  match StringMap.find_opt name map with
                  | Some env -> env
                  | None -> []
                in
                List.fold_left
                  (fun env subst -> List.filter (( <> ) subst.target) env)
                  base_env entries
            | _ -> [])
        | _ -> []
      in
      StringMap.add binding.var new_env map)
    env_map clause
%}

%token <string> IDENT
%token TYPE FUNCTION BINDER RULE STRATEGY
%token COLON DOT SEMI PIPE COMMA PIPEPIPE
%token LPAREN RPAREN
%token LBRACE RBRACE LBRACKET RBRACKET
%token EQ LARROW AT
%token WHERE
%token DO
%token LIMIT DEPTH
%token ARROWBIG ARROWDASH ARROWX
%token ARROW
%token STAR QUESTION
%token <int> INT
%token EOF

%start file
%start problem
%type <Ast.t> file
%type <Problem.t> problem

%type <t> items
%type <t -> t> entry
%type <Ast.expr> expr
%type <Ast.expr list> expr_list
%type <Ast.expr list> lhs_expr_list
%type <Ast.expr list list> rhs
%type <Ast.strategy_decl> strat_expr
%type <Ast.rule_type> arrow
%type <Ast.function_decl list> problem_functions
%type <Ast.function_decl list> problem_function
%type <string list> ident_seq
%type <Ast.expr list> problem_formulas
%type <Ast.expr list> problem_formulas_rest
%type <Ast.tree_expr> tree_expr
%type <Ast.tree_expr> tree_atom
%type <Ast.tree_expr> tree_union
%type <Ast.expr list> branch_expr
%type <Ast.expr list> branch_expr_tail
%type <Ast.where_binding list> where_opt
%type <Ast.where_binding list> where_entries
%type <Ast.where_binding list> where_entries_tail
%type <Ast.where_binding> where_entry
%type <Ast.where_expr> where_expr
%type <Ast.where_base> where_base
%type <Ast.where_subst option> where_substs_opt
%type <Ast.where_subst> substitution_body
%type <Ast.subst_entry list> substitution_list
%type <Ast.subst_entry> substitution_assignment
%type <Ast.subst_rhs> substitution_rhs
%type <Ast.generator_call> generator_call
%type <Ast.expr list> generator_args_opt
%type <Ast.expr list> generator_args

%%

file:
  | items EOF {
      {
        types     = List.rev $1.types;
        functions = List.rev $1.functions;
        binders   = List.rev $1.binders;
        rules     = List.rev $1.rules;
        strategies  = List.rev $1.strategies;
      }
    }

items:
  | /* empty */ { empty_acc }
  | items entry { $2 $1 }

entry:
  | TYPE IDENT {
      fun acc -> { acc with types = $2 :: acc.types }
    }
  | FUNCTION IDENT COLON type_list ARROW IDENT {
      fun acc ->
        let fd = { name = $2; params_types = $4; t = $6 } in
        { acc with functions = fd :: acc.functions }
    }
  | BINDER IDENT COLON IDENT DOT IDENT {
      fun acc ->
        let bd = { name = $2; variable_type = $4; t = $6 } in
        { acc with binders = bd :: acc.binders }
    }
  | RULE IDENT COLON lhs_expr_list arrow rhs_opt where_opt {
      fun acc ->
        let where_clause = $7 in
        let lhs_info = collect_lhs $4 in
        check_rule $2 lhs_info $6 where_clause;
        let extended_env =
          extend_env_map lhs_info.env_map where_clause |> StringMap.bindings
        in
        let rd =
          {
            name = $2;
            lhs = $4;
            arrow = $5;
            rhs = $6;
            where_clause;
            meta_envs = extended_env;
            tree_rule = None;
          }
        in
        { acc with rules = rd :: acc.rules }
    }
  | RULE IDENT COLON tree_union arrow tree_expr where_opt {
      fun acc ->
        let where_clause = $7 in
        let lhs_tree = $4 in
        let rd =
          {
            name = $2;
            lhs = [];
            arrow = $5;
            rhs = [];
            where_clause;
            meta_envs = [];
            tree_rule =
              Some
                {
                  Ast.lhs_tree = lhs_tree;
                  rhs_tree = $6;
                  branch_tail = None;
                  tree_var = "";
                };
          }
        in
        { acc with rules = rd :: acc.rules }
    }
  | STRATEGY IDENT COLON strat_expr {
      fun acc ->
        { acc with strategies = ($2, $4) :: acc.strategies }
    }

type_list:
  | { [] }
  | IDENT { [$1] }
  | type_list IDENT { $1 @ [$2] }

arrow:
  | ARROWBIG { Invertible }
  | ARROWX   { Close }
  | ARROW    { NoInvertible }
  | ARROWDASH { NoInvertible }

rhs_opt:
  | /* empty */ { [] }            /* allow no RHS, useful for ==X */
  | rhs { $1 }

rhs:
  | rhs_alt { [$1] }
  | rhs PIPE rhs_alt { $1 @ [$3] }

rhs_alt:
  | expr_list { $1 }

lhs_expr_list:
  | lhs_expr { [$1] }
  | lhs_expr SEMI lhs_expr_list { $1 :: $3 }

expr_list:
  | expr { [$1] }
  | expr SEMI expr_list { $1 :: $3 }

lhs_expr:
  | IDENT LPAREN args_opt RPAREN { LFun ($1, $3) }
  | IDENT IDENT DOT expr { LBinder ($1, $2, $4) }
  | IDENT { LVar $1 }

expr:
  | IDENT LPAREN args_opt RPAREN { LFun ($1, $3) }
  | IDENT IDENT DOT expr { LBinder ($1, $2, $4) }
  | LPAREN expr RPAREN { $2 }
  | IDENT { LVar $1 }

args_opt:
  | /* empty */ { [] }
  | args { $1 }

args:
  | expr { [$1] }
  | expr COMMA args { $1 :: $3 }

strat_expr:
  | strat_seq { $1 }

strat_seq:
  | strat_or { $1 }
  | strat_seq SEMI strat_or { AndThen ($1, $3) }

strat_or:
  | strat_post { $1 }
  | strat_or PIPEPIPE strat_post { OrElse ($1, $3) }

strat_post:
  | strat_atom { $1 }
  | strat_post STAR { Repeat $1 }
  | strat_post QUESTION { Try $1 }

strat_atom:
  | IDENT { Rule $1 }
  | LPAREN strat_expr RPAREN { $2 }
  | DO INT strat_atom { Do ($2, $3) }
  | LIMIT INT strat_atom { Limit (Ast.LimitConst $2, $3) }
  | LIMIT DEPTH strat_atom { Limit (Ast.LimitDepth, $3) }
  | DEPTH strat_atom { Depth $2 }

problem:
  | problem_functions problem_formulas EOF {
      Problem.of_components $1 $2
    }

problem_functions:
  | /* empty */ { [] }
  | problem_functions problem_function { $1 @ $2 }

problem_function:
  | FUNCTION ident_seq COLON type_list ARROW IDENT {
      List.map
        (fun name -> { name; params_types = $4; t = $6 })
        $2
    }
  | FUNCTION ident_seq COLON ARROW IDENT {
      List.map (fun name -> { name; params_types = []; t = $5 }) $2
    }

ident_seq:
  | IDENT { [$1] }
  | ident_seq IDENT { $1 @ [$2] }

problem_formulas:
  | expr problem_formulas_rest { $1 :: $2 }

problem_formulas_rest:
  | SEMI problem_formulas { $2 }
  | SEMI { [] }
  | /* empty */ { [] }

where_opt:
  | WHERE LBRACE where_entries RBRACE { ensure_unique_bindings $3 }
  | /* empty */ { empty_where }

where_entries:
  | /* empty */ { [] }
  | where_entry where_entries_tail { $1 :: $2 }

where_entries_tail:
  | /* empty */ { [] }
  | SEMI where_entries { $2 }
  | SEMI { [] }

where_entry:
  | IDENT EQ where_expr { { var = $1; value = $3 } }

where_expr:
  | where_base where_substs_opt { { base = $1; substs = $2 } }

where_substs_opt:
  | /* empty */ { None }
  | LBRACKET substitution_body RBRACKET { Some $2 }

where_base:
  | tree_expr {
      match $1 with
      | Ast.TreeLeaf e -> Ast.WExpr e
      | tree -> Ast.WTree tree
    }

substitution_body:
  | substitution_list { Ast.SubstEntries $1 }
  | expr { Ast.SubstRef $1 }

substitution_list:
  | substitution_assignment { [$1] }
  | substitution_assignment COMMA substitution_list { $1 :: $3 }

substitution_assignment:
  | IDENT LARROW substitution_rhs { { target = $1; rhs = $3 } }

substitution_rhs:
  | generator_call { SR_Gen $1 }
  | where_expr { SR_Expr $1 }

generator_call:
  | AT IDENT generator_args_opt { { gen_name = $2; gen_args = $3 } }

generator_args_opt:
  | LPAREN RPAREN { [] }
  | LPAREN generator_args RPAREN { $2 }
  | /* empty */ { [] }

generator_args:
  | expr { [$1] }
  | expr COMMA generator_args { $1 :: $3 }

tree_expr:
  | tree_atom { $1 }
  | tree_expr PIPE tree_atom { Ast.TreeUnion ($1, $3) }

tree_union:
  | tree_atom PIPE tree_atom { Ast.TreeUnion ($1, $3) }
  | tree_union PIPE tree_atom { Ast.TreeUnion ($1, $3) }

tree_atom:
  | LPAREN tree_expr RPAREN { $2 }
  | LPAREN branch_expr RPAREN { Ast.TreeBranch $2 }
  | expr { Ast.TreeLeaf $1 }

branch_expr:
  | expr branch_expr_tail { $1 :: $2 }

branch_expr_tail:
  | SEMI branch_expr { $2 }
  | /* empty */ { [] }

%%
