%{
open Ast

let empty_acc = {
  types = [];
  functions = [];
  binders = [];
  rules = [];
  strategies = [];
}
%}

%token <string> IDENT
%token TYPE FUNCTION BINDER RULE STRATEGY
%token COLON DOT SEMI PIPE COMMA PIPEPIPE
%token LPAREN RPAREN
%token ARROWBIG ARROWDASH ARROWX
%token ARROW
%token BANG QUESTION
%token EOF

%start file
%start problem
%type <Ast.t> file
%type <Problem.t> problem

%type <t> items
%type <t -> t> entry
%type <Ast.expr> expr
%type <Ast.expr list> expr_list
%type <Ast.expr list list> rhs
%type <Ast.strategy_decl> strat_expr
%type <Ast.rule_type> arrow
%type <Ast.function_decl list> problem_functions
%type <Ast.function_decl list> problem_function
%type <string list> ident_seq
%type <Ast.expr list> problem_formulas
%type <Ast.expr list> problem_formulas_rest

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
  | RULE IDENT COLON expr_list arrow rhs_opt {
      fun acc ->
        let rd = { name = $2; lhs = $4; arrow = $5; rhs = $6 } in
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

expr_list:
  | expr { [$1] }
  | expr SEMI expr_list { $1 :: $3 }

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
  | strat_post BANG { Repeat $1 }
  | strat_post QUESTION { Try $1 }

strat_atom:
  | IDENT { Rule $1 }
  | LPAREN strat_expr RPAREN { $2 }

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

%%
