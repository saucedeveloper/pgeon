val compile :
  Registry.t ->
  Ast.logic_decl ->
  Ast.problem_decl ->
  Tableau.proof_state * Tableau.strategy
