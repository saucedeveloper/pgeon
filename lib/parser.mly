%{
open Ast2

type logic_acc = {
  types : string list;
  main_type : string option;
  functions : function_decl list;
  binders : binder_decl list;
  rules : rule_decl list;
  strategies : strategy_decl list;
  main_strategy : string option;
}

let empty_logic =
  {
    types = [];
    main_type = None;
    functions = [];
    binders = [];
    rules = [];
    strategies = [];
    main_strategy = None;
  }

let finalize_logic (acc : logic_acc) : logic_file =
  let main_type =
    match acc.main_type with
    | Some t -> t
    | None -> failwith "missing main type declaration (use 'main type <name>')"
  in
  let main_strategy =
    match acc.main_strategy with
    | Some s -> s
    | None ->
        failwith
          "missing main strategy declaration (use 'main strategy <name> : ...')"
  in
  {
    types = List.rev acc.types;
    main_type;
    functions = List.rev acc.functions;
    binders = List.rev acc.binders;
    rules = List.rev acc.rules;
    strategies = List.rev acc.strategies;
    main_strategy;
  }

let expr_to_string expr =
  let rec aux = function
    | EVar v -> v
    | EApp (name, []) -> Printf.sprintf "%s()" name
    | EApp (name, args) ->
        let args = List.map aux args |> String.concat ", " in
        Printf.sprintf "%s(%s)" name args
    | EBind (binder, var, body) ->
        Printf.sprintf "%s %s. %s" binder var (aux body)
    | EBranchTail name -> Printf.sprintf "...%s" name
    | ETreeTail name -> Printf.sprintf "...%s" name
  in
  aux expr

let expect_var = function
  | EVar v -> v
  | _ -> failwith "expected meta variable in mge substitution"
%}

%token <string> IDENT
%token TREE TYPE FUNCTION BINDER RULE STRATEGY WHERE MAIN
%token COLON DOT SEMI PIPE PIPEPIPE COMMA
%token LPAREN RPAREN
%token LBRACE RBRACE LBRACKET RBRACKET
%token EQ ARROWBIG ARROWDASH ARROWX ARROW
%token LARROW
%token STAR QUESTION BANG
%token ELLIPSIS
%token EOF

%start logic_file
%start problem_file

%type <Ast2.logic_file> logic_file
%type <Ast2.problem_file> problem_file

%type <logic_acc> logic_items
%type <logic_acc -> logic_acc> logic_entry
%type <string list * string> func_type
%type <string list> type_seq ident_list
%type <Ast2.expr list> expr_sequence
%type <Ast2.expr list> expr_branch
%type <Ast2.expr list list> branch_list rhs_branches rhs_branches_opt
%type <Ast2.expr list> branch_item
%type <Ast2.expr list list> lhs_tree lhs_tree_body
%type <Ast2.expr> expr
%type <Ast2.expr list> args
%type <Ast2.expr list> args_opt
%type <Ast2.strategy_kind> strat_expr strat_or strat_then strat_postfix strat_atom
%type <Ast2.where_binding list> where_clause_opt where_entries
%type <Ast2.where_binding> where_entry
%type <Ast2.where_expr> where_expr
%type <Ast2.where_subst> where_subst where_subst_opt
%type <Ast2.where_subst_entry list> substitution_entries_tail substitution_list
%type <Ast2.where_subst_entry> substitution_entry
%type <bool> rule_arrow
%type <Ast2.function_decl list> function_decl_item
%type <Ast2.function_decl list> problem_functions
%type <Ast2.function_decl list> problem_functions_rev
%type <Ast2.expr list> problem_formulas
%type <Ast2.expr list> nonempty_problem_formulas
%type <unit> opt_semi

%left PIPEPIPE
%left SEMI
%right STAR QUESTION BANG

%%

logic_file:
  | logic_items EOF { finalize_logic $1 }

logic_items:
  | /* empty */ { empty_logic }
  | logic_items logic_entry { $2 $1 }

logic_entry:
  | MAIN TYPE IDENT {
      fun acc ->
        (match acc.main_type with
        | Some existing ->
            failwith
              (Printf.sprintf
                 "multiple main type declarations: %s and %s"
                 existing $3)
        | None -> ());
        {
          acc with
          types = $3 :: acc.types;
          main_type = Some $3;
        }
    }
  | TYPE IDENT {
      fun acc -> { acc with types = $2 :: acc.types }
    }
  | FUNCTION ident_list COLON func_type {
      fun acc ->
        let params, ret = $4 in
        let decls =
          List.rev_map
            (fun name -> { name; params_types = params; t = ret })
            $2
        in
        { acc with functions = List.rev_append decls acc.functions }
    }
  | BINDER IDENT COLON IDENT DOT IDENT {
      fun acc ->
        let decl = { name = $2; variable_type = $4; t = $6 } in
        { acc with binders = decl :: acc.binders }
    }
  | RULE IDENT COLON expr_branch ARROWX {
      fun acc ->
        let rule = RuleClosure { name = $2; lhs = $4 } in
        { acc with rules = rule :: acc.rules }
    }
  | TREE RULE IDENT COLON lhs_tree rule_arrow rhs_branches where_clause_opt {
      fun acc ->
        let rule =
          RuleTree
            {
              name = $3;
              lhs = $5;
              rhs = $7;
              is_invertible = $6;
              where_clause = $8;
            }
        in
        { acc with rules = rule :: acc.rules }
    }
  | RULE IDENT COLON lhs_tree rule_arrow rhs_branches where_clause_opt {
      fun acc ->
        let rule =
          RuleTree
            {
              name = $2;
              lhs = $4;
              rhs = $6;
              is_invertible = $5;
              where_clause = $7;
            }
        in
        { acc with rules = rule :: acc.rules }
    }
  | RULE IDENT COLON expr_branch rule_arrow rhs_branches_opt where_clause_opt {
      fun acc ->
        let rule =
          RuleBranch
            {
              name = $2;
              lhs = $4;
              rhs = $6;
              is_invertible = $5;
              where_clause = $7;
            }
        in
        { acc with rules = rule :: acc.rules }
    }
  | STRATEGY IDENT COLON strat_expr {
      fun acc ->
        let decl = { name = $2; strategy = $4 } in
        { acc with strategies = decl :: acc.strategies }
    }
  | MAIN STRATEGY IDENT COLON strat_expr {
      fun acc ->
        (match acc.main_strategy with
        | Some existing ->
            failwith
              (Printf.sprintf
                 "multiple main strategy declarations: %s and %s"
                 existing $3)
        | None -> ());
        let decl = { name = $3; strategy = $5 } in
        {
          acc with
          strategies = decl :: acc.strategies;
          main_strategy = Some $3;
        }
    }

func_type:
  | ARROW IDENT { ([], $2) }
  | type_seq ARROW IDENT { ($1, $3) }

type_seq:
  | IDENT { [$1] }
  | type_seq IDENT { $1 @ [$2] }

ident_list:
  | IDENT { [$1] }
  | IDENT ident_list { $1 :: $2 }

lhs_tree:
  | lhs_tree_body { $1 }
  | LPAREN lhs_tree_body RPAREN { $2 }

lhs_tree_body:
  | expr_branch PIPE branch_list { $1 :: $3 }

rhs_branches_opt:
  | rhs_branches { $1 }
  | /* empty */ { [] }

rhs_branches:
  | branch_list { $1 }

branch_list:
  | branch_item { [$1] }
  | branch_item PIPE branch_list { $1 :: $3 }

branch_item:
  | ELLIPSIS IDENT { [ETreeTail $2] }
  | expr_branch { $1 }

expr_branch:
  | expr_sequence { $1 }
  | LPAREN expr_sequence RPAREN { $2 }

expr_sequence:
  | expr { [$1] }
  | expr SEMI expr_sequence { $1 :: $3 }
  | expr SEMI ELLIPSIS IDENT { [ $1; EBranchTail $4 ] }

expr:
  | IDENT LPAREN args_opt RPAREN { EApp ($1, $3) }
  | IDENT IDENT DOT expr { EBind ($1, $2, $4) }
  | LPAREN expr RPAREN { $2 }
  | IDENT { EVar $1 }

args_opt:
  | /* empty */ { [] }
  | args { $1 }

args:
  | expr { [$1] }
  | expr COMMA args { $1 :: $3 }

rule_arrow:
  | ARROWBIG { true }
  | ARROWDASH { false }
  | ARROW { false }

where_clause_opt:
  | WHERE LBRACE where_entries RBRACE { $3 }
  | /* empty */ { [] }

where_entries:
  | /* empty */ { [] }
  | where_entry where_entries { $1 :: $2 }

where_entry:
  | IDENT EQ where_expr { { var = $1; value = $3 } }

where_expr:
  | branch_list where_subst_opt {
      { expr = $1; subst = $2 }
    }
  | LPAREN branch_list RPAREN where_subst_opt {
      { expr = $2; subst = $4 }
    }

where_subst_opt:
  | LBRACKET where_subst RBRACKET { $2 }
  | /* empty */ { WSubstEntries [] }

where_subst:
  | IDENT LARROW expr substitution_entries_tail {
      let first = { var = $1; gen = expr_to_string $3 } in
      WSubstEntries (first :: $4)
    }
  | IDENT LPAREN expr COMMA expr RPAREN {
      match String.equal $1 "mge", $3, $5 with
      | true, lhs, rhs ->
          let var1 = expect_var lhs in
          let var2 = expect_var rhs in
          WSMge { var1; var2 }
      | _ -> failwith "unsupported substitution form"
    }

substitution_entries_tail:
  | /* empty */ { [] }
  | COMMA substitution_list { $2 }

substitution_list:
  | substitution_entry { [$1] }
  | substitution_entry COMMA substitution_list { $1 :: $3 }

substitution_entry:
  | IDENT LARROW expr {
      { var = $1; gen = expr_to_string $3 }
    }

strat_expr:
  | strat_or { $1 }

strat_or:
  | strat_or PIPEPIPE strat_then { SOr ($1, $3) }
  | strat_then { $1 }

strat_then:
  | strat_then SEMI strat_postfix { SThen ($1, $3) }
  | strat_postfix { $1 }

strat_postfix:
  | strat_postfix STAR { SRepeat $1 }
  | strat_postfix QUESTION { STry $1 }
  | strat_atom { $1 }

strat_atom:
  | IDENT BANG { SRuleBang $1 }
  | IDENT { SCall $1 }
  | LPAREN strat_expr RPAREN { $2 }

problem_file:
  | problem_functions problem_formulas EOF {
      { functions = $1; formulas = $2 }
    }

problem_functions:
  | problem_functions_rev { List.rev $1 }

problem_functions_rev:
  | /* empty */ { [] }
  | problem_functions_rev function_decl_item { List.rev_append $2 $1 }

function_decl_item:
  | FUNCTION ident_list COLON func_type {
      let params, ret = $4 in
      List.map (fun name -> { name; params_types = params; t = ret }) $2
    }

problem_formulas:
  | /* empty */ { [] }
  | nonempty_problem_formulas { $1 }

nonempty_problem_formulas:
  | expr opt_semi { [$1] }
  | expr SEMI nonempty_problem_formulas { $1 :: $3 }

opt_semi:
  | /* empty */ { () }
  | SEMI { () }
