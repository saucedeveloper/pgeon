let string_of_term t =
  if Log.get_level () > Log.Debug then ""
  else
    let rec string_of_term = function
      | Term.Bvar i -> Printf.sprintf "#%d" i
      | Term.Fvar x -> "'" ^ x
      | Term.Mvar x -> "?" ^ x
      | Term.App (f, []) -> f
      | Term.App (f, args) ->
          Printf.sprintf "%s(%s)" f
            (String.concat ", " (List.map string_of_term args))
      | Term.Bind (b, body) -> Printf.sprintf "%s.(%s)" b (string_of_term body)
    in
    string_of_term t

let string_of_free_subst (Term.FreeSubstitution subst) =
  if Log.get_level () > Log.Debug then ""
  else
    let items =
      subst
      |> List.map (fun (x, t) -> Printf.sprintf "%s <- %s" x (string_of_term t))
    in
    "{ " ^ String.concat ", " items ^ " }"

let string_of_formula (id, t) =
  if Log.get_level () > Log.Debug then ""
  else Printf.sprintf "%d:%s" id (string_of_term t)

let string_of_branch br =
  if Log.get_level () > Log.Debug then ""
  else "[" ^ String.concat " ; " (List.map string_of_formula br) ^ "]"

let string_of_tree tree =
  if Log.get_level () > Log.Debug then ""
  else
    String.concat "\n"
      (List.mapi
         (fun i br -> Printf.sprintf "  branch %d = %s" i (string_of_branch br))
         tree)

let term_of_expr expr =
  let rec compile env = function
    | Ast.EVar v -> (
        match List.find_index (( = ) v) env with
        | Some i -> Term.Bvar i
        | None ->
            if String.capitalize_ascii v = v then Term.Mvar v
            else Term.App (v, []))
    | Ast.EApp (f, args) -> Term.App (f, List.map (compile env) args)
    | Ast.EBind (b, v, body) -> Term.Bind (b, compile (v :: env) body)
  in
  compile [] expr

let free_substitute_tree theta (tree : Tableau.proof_tree) : Tableau.proof_tree
    =
  List.map (List.map (fun (id, t) -> (id, Term.free_substitute theta t))) tree

let _meta_substitute_tree subst (tree : Tableau.proof_tree) : Tableau.proof_tree
    =
  List.map (List.map (fun (id, t) -> (id, Term.substitute subst t))) tree

type rule_env = (string * Term.t) list
type branch_env = (string * Tableau.formula list) list
type tree_env = (string * Tableau.proof_tree) list

let branch_tail_name = function
  | None -> None
  | Some (Ast.TailAny name) | Some (Ast.TailMapped (_, name)) -> Some name

let instantiate_rule_expr lhs_subst (env : rule_env) (expr : Ast.expr) =
  let rec go : Ast.expr -> Term.t = function
    | EVar v -> (
        match List.assoc_opt v env with
        | Some t -> t
        | None -> Term.substitute lhs_subst (Term.Mvar v))
    | EApp (f, args) -> Term.App (f, List.map go args)
    | t (* EBind *) ->
        let rec compile env_names : Ast.expr -> Term.t = function
          | EVar x -> (
              match List.find_index (( = ) x) env_names with
              | Some i -> Term.Bvar i
              | None -> (
                  match List.assoc_opt x env with
                  | Some t -> t
                  | None -> Term.substitute lhs_subst (Term.Mvar x)))
          | EApp (f, args) -> Term.App (f, List.map (compile env_names) args)
          | EBind (b, x, body) -> Term.Bind (b, compile (x :: env_names) body)
        in
        compile [] t
  in
  go expr

let instantiate_rule_expr_from_term lhs_subst (env : rule_env) t =
  let rec go = function
    | Term.Bvar _ as t -> t
    | Term.Fvar _ as t -> t
    | Term.Mvar v -> (
        match List.assoc_opt v env with
        | Some t -> t
        | None -> Term.substitute lhs_subst (Term.Mvar v))
    | Term.App (f, args) -> Term.App (f, List.map go args)
    | Term.Bind (b, body) -> Term.Bind (b, go body)
  in
  go t

let eval_where_op runtime st lhs_subst env src_term = function
  | Ast.WhereSubstGen { bound = _; by } -> (
      match (Registry.find_generator runtime by.name) st src_term with
      | None -> None
      | Some (generated, st') ->
          let result = Term.var_open src_term generated in
          Some (st', result))
  | Ast.WhereUnifier { name; left; right } -> (
      let left_t = instantiate_rule_expr lhs_subst env left in
      let right_t = instantiate_rule_expr lhs_subst env right in
      match (Registry.find_unifier runtime name) left_t right_t with
      | None -> None
      | Some theta ->
          let result = Term.free_substitute theta src_term in
          Some (st, result))

let add_terms_to_branch start_id terms branch =
  let rec aux next_id acc = function
    | [] -> (List.rev acc @ branch, next_id)
    | t :: tl -> aux (next_id + 1) ((next_id, t) :: acc) tl
  in
  aux start_id [] terms

let instantiate_branch_expr subst env branch_env next_formula_id
    ((exprs, tail) : Ast.branch_expr) =
  let base_branch =
    match branch_tail_name tail with
    | None -> Some []
    | Some tail_var -> List.assoc_opt tail_var branch_env
  in
  match base_branch with
  | None -> None
  | Some base_branch ->
      let terms = List.map (instantiate_rule_expr subst env) exprs in
      let branch, next_formula_id =
        add_terms_to_branch next_formula_id terms base_branch
      in
      Some (branch, next_formula_id)

let pattern_matches pattern term =
  let rec go subst pattern term =
    match pattern with
    | Term.Mvar "_" -> Some subst
    | Term.Mvar m -> (
        match List.assoc_opt m subst with
        | None -> Some ((m, term) :: subst)
        | Some bound -> if bound = term then Some subst else None)
    | Term.Bvar i -> (
        match term with Term.Bvar j when i = j -> Some subst | _ -> None)
    | Term.Fvar x -> (
        match term with Term.Fvar y when x = y -> Some subst | _ -> None)
    | Term.App (f, ps) -> (
        match term with
        | Term.App (g, ts) when f = g && List.length ps = List.length ts ->
            List.fold_left2
              (fun acc p t -> Option.bind acc (fun subst -> go subst p t))
              (Some subst) ps ts
        | _ -> None)
    | Term.Bind (b, p) -> (
        match term with
        | Term.Bind (b', t) when b = b' -> go subst p t
        | _ -> None)
  in
  Option.is_some (go [] pattern term)

let instantiate_tree_expr subst env branch_env tree_env next_formula_id
    ((branches, tree_tail) : Ast.tree_expr) =
  match List.assoc_opt tree_tail tree_env with
  | None -> None
  | Some tail_tree ->
      let rec aux next_formula_id acc = function
        | [] -> Some (List.rev acc @ tail_tree, next_formula_id)
        | br :: tl -> (
            match
              instantiate_branch_expr subst env branch_env next_formula_id br
            with
            | None -> None
            | Some (branch, next_formula_id') ->
                aux next_formula_id' (branch :: acc) tl)
      in
      aux next_formula_id [] branches

let eval_where_expr_clause runtime st lhs_subst (env : rule_env)
    (branch_env : branch_env) (tree_env : tree_env) = function
  | Ast.WhereExprClause { dst; src; op } ->
      let src_term = instantiate_rule_expr lhs_subst env src in
      eval_where_op runtime st lhs_subst env src_term op
      |> Option.map (fun (st', result) ->
          (st', (dst, result) :: env, branch_env, tree_env))
  | Ast.WhereTreeClause { dst; src; op } -> (
      match
        instantiate_tree_expr lhs_subst env branch_env tree_env
          st.next_formula_id src
      with
      | None -> None
      | Some (src_tree, _) -> (
          match op with
          | Ast.WhereSubstGen _ ->
              failwith "Tree generation not supported in where clauses"
          | Ast.WhereUnifier { name; left; right } -> (
              let left_t = instantiate_rule_expr lhs_subst env left in
              let right_t = instantiate_rule_expr lhs_subst env right in
              match (Registry.find_unifier runtime name) left_t right_t with
              | None -> None
              | Some theta ->
                  let result_tree = free_substitute_tree theta src_tree in
                  Log.debug
                    "TREE WHERE %s\nsrc_tree=\n%s\ntheta=%s\nresult_tree=\n%s\n"
                    dst (string_of_tree src_tree)
                    (string_of_free_subst theta)
                    (string_of_tree result_tree);
                  Some (st, env, branch_env, (dst, result_tree) :: tree_env))))
  | Ast.WhereBranchAllMatch { branch; pattern } -> (
      match List.assoc_opt branch branch_env with
      | None -> None
      | Some formulas ->
          let pattern = instantiate_rule_expr lhs_subst env pattern in
          if List.for_all (fun (_, term) -> pattern_matches pattern term) formulas
          then Some (st, env, branch_env, tree_env)
          else None)

let eval_where_clauses (reg : Registry.t) st
    (lhs_subst : Term.meta_substitution) (branch_env : branch_env)
    (tree_env : tree_env) clauses =
  let rec go st env branch_env tree_env = function
    | [] -> Some (st, env, branch_env, tree_env)
    | clause :: tl -> (
        match
          eval_where_expr_clause reg st lhs_subst env branch_env tree_env clause
        with
        | None -> None
        | Some (st', env', branch_env', tree_env') ->
            go st' env' branch_env' tree_env' tl)
  in
  go st [] branch_env tree_env clauses

let cache_key (rule_id : int) (candidate : Tableau.formula list) =
  (rule_id, List.sort compare (List.map fst candidate))

(* let cache_check cache key = List.mem key cache *)
let cache_check _ _ = false

let cache_insert cache key =
  if cache_check cache key then cache else key :: cache

let compile_rule (reg : Registry.t) id (decl : Ast.rule_decl) : Tableau.rule =
  let build_branches start_id base_branch rhs_branches =
    match rhs_branches with
    | [] -> ([ base_branch ], start_id)
    | _ ->
        let rec aux next_id acc = function
          | [] -> (List.rev acc, next_id)
          | rhs_branch :: tl ->
              let new_branch, next_id' =
                add_terms_to_branch next_id rhs_branch base_branch
              in
              aux next_id' (new_branch :: acc) tl
        in
        aux start_id [] rhs_branches
  in
  let generate_candidates_branch_rules (t : Tableau.proof_tree) (arity : int) =
    t |> List.to_seq
    |> Seq.mapi (fun branch_idx branch -> (branch_idx, branch))
    |> Seq.flat_map (fun (branch_idx, branch) ->
        Utils.perm arity branch |> List.to_seq
        |> Seq.map (fun candidate ->
            let candidate_ids = List.map fst candidate in
            let rest_branch =
              List.filter
                (fun (formula_id, _) -> not (List.mem formula_id candidate_ids))
                branch
            in
            let rest_tree =
              t
              |> List.mapi (fun i br -> (i, br))
              |> List.filter (fun (i, _) -> i <> branch_idx)
              |> List.map snd
            in
            (rest_tree, rest_branch, candidate)))
  in
  let generate_candidates_tree_rules (t : Tableau.proof_tree)
      (lhs_branches : Ast.branch_expr list) =
    let indexed_tree = List.mapi (fun i branch -> (i, branch)) t in
    let rec choose_formulas acc = function
      | [], [] -> Seq.return (List.rev acc)
      | (_, branch) :: branch_tl, ((exprs, _) as lhs_branch) :: lhs_tl ->
          let arity = List.length exprs in
          Utils.perm arity branch |> List.to_seq
          |> Seq.map (fun candidate ->
              let candidate_ids = List.map fst candidate in
              let rest_branch =
                List.filter
                  (fun (formula_id, _) ->
                    not (List.mem formula_id candidate_ids))
                  branch
              in
              (lhs_branch, rest_branch, candidate))
          |> Seq.flat_map (fun matched ->
              choose_formulas (matched :: acc) (branch_tl, lhs_tl))
      | _ -> Seq.empty
    in
    Utils.perm (List.length lhs_branches) indexed_tree
    |> List.to_seq
    |> Seq.flat_map (fun selected_branches ->
        let selected_ids = List.map fst selected_branches in
        let rest_tree =
          indexed_tree
          |> List.filter (fun (i, _) -> not (List.mem i selected_ids))
          |> List.map snd
        in
        choose_formulas [] (selected_branches, lhs_branches)
        |> Seq.map (fun matched_branches -> (rest_tree, matched_branches)))
  in
  match decl with
  | Ast.RuleBranch { arrow; lhs; rhs; where; name } ->
      let lhs_terms = List.map term_of_expr lhs in
      let rhs_termss = List.map (List.map term_of_expr) rhs in
      let arity = List.length lhs_terms in
      {
        id;
        run =
          (fun st ->
            generate_candidates_branch_rules st.tree arity
            |> Seq.filter_map (fun (rest_tree, rest_branch, candidate) ->
                let candidate_terms = List.map snd candidate in
                match Term.match_terms candidate_terms lhs_terms with
                | None -> None
                | Some lhs_subst -> (
                    Log.debug
                      "RULE %s matched\n\
                       candidate=%s\n\
                       rest_branch=%s\n\
                       rest_tree=\n\
                       %s\n"
                      name
                      (string_of_branch candidate)
                      (string_of_branch rest_branch)
                      (string_of_tree rest_tree);
                    match eval_where_clauses reg st lhs_subst [] [] where with
                    | None ->
                        Log.debug "RULE %s: where clauses failed\n" name;
                        None
                    | Some (st, env, _, _) -> (
                        let rhs_instantiated =
                          List.map
                            (List.map
                               (instantiate_rule_expr_from_term lhs_subst env))
                            rhs_termss
                        in
                        match arrow with
                        | Ast.Close ->
                            Log.debug
                              "CLOSE by rule %s on candidate=%s\n\
                               result tree=\n\
                               %s\n"
                              name
                              (string_of_branch candidate)
                              (string_of_tree rest_tree);
                            Some { st with tree = rest_tree }
                        | Ast.Invertible ->
                            let new_branches, next_formula_id =
                              build_branches st.next_formula_id rest_branch
                                rhs_instantiated
                            in
                            Log.debug
                              "INVERTIBLE rule %s applied on candidate=%s\n\
                               result tree=\n\
                               %s\n"
                              name
                              (string_of_branch candidate)
                              (string_of_tree (new_branches @ rest_tree));
                            Some
                              {
                                st with
                                tree = new_branches @ rest_tree;
                                next_formula_id;
                              }
                        | Ast.NonInvertible ->
                            let key = cache_key id candidate in
                            if cache_check st.applied key then None
                            else
                              let base_branch = candidate @ rest_branch in
                              let new_branches, next_formula_id =
                                build_branches st.next_formula_id base_branch
                                  rhs_instantiated
                              in
                              Log.debug
                                "NON-INVERTIBLE rule %s applied on candidate=%s\n\
                                 result tree=\n\
                                 %s\n"
                                name
                                (string_of_branch candidate)
                                (string_of_tree (new_branches @ rest_tree));
                              Some
                                {
                                  st with
                                  tree = new_branches @ rest_tree;
                                  next_formula_id;
                                  applied = cache_insert st.applied key;
                                }))));
        run_bang = Some (fun _ -> failwith "TODO: implement run_bang for branch rules") (* TODO *);
      }
  | Ast.RuleTree { arrow = _; lhs = lhs_branches, lhs_tree_tail; rhs; where; name }
    ->
      {
        id;
        run =
          (fun st ->
            generate_candidates_tree_rules st.tree lhs_branches
            |> Seq.filter_map (fun (rest_tree, matched_branches) ->
                let candidate_terms =
                  matched_branches
                  |> List.concat_map (fun (_, _, candidate) ->
                      List.map snd candidate)
                in
                let lhs_terms =
                  matched_branches
                  |> List.concat_map (fun ((exprs, _), _, _) ->
                      List.map term_of_expr exprs)
                in
                match Term.match_terms candidate_terms lhs_terms with
                | None -> None
                | Some subst ->
                    let branch_env =
                      matched_branches
                      |> List.filter_map
                           (fun ((_, tail), rest_branch, _candidate) ->
                             match tail with
                             | None ->
                                 if rest_branch = [] then Some [] else None
                             | Some (Ast.TailAny tail_var) ->
                                 Some [ (tail_var, rest_branch) ]
                             | Some (Ast.TailMapped (f, tail_var)) ->
                                 if
                                   List.for_all
                                     (function
                                       | _, Term.App (g, [ _ ]) -> f = g
                                       | _ -> false)
                                     rest_branch
                                 then Some [ (tail_var, rest_branch) ]
                                 else None)
                    in
                    if List.length branch_env <> List.length matched_branches
                    then None
                    else
                      let branch_env = List.concat branch_env in
                      let tree_env = [ (lhs_tree_tail, rest_tree) ] in
                      match
                        eval_where_clauses reg st subst branch_env tree_env
                          where
                      with
                      | None -> None
                      | Some (st, env, branch_env, tree_env) -> (
                          match
                            instantiate_tree_expr subst env branch_env tree_env
                              st.next_formula_id rhs
                          with
                          | None -> None
                          | Some (tree, next_formula_id) ->
                              Log.debug
                                "TREE RULE %s succeeded\nnew_tree=\n%s\n" name
                                (string_of_tree tree);
                              Some { st with tree; next_formula_id })));
        run_bang = None;
      }

let compile (reg : Registry.t) (logic : Ast.logic_decl)
    (problem : Ast.problem_decl) =
  let compiled_rules =
    logic.rules
    |> List.mapi (fun id rule ->
        let name =
          match rule with
          | Ast.RuleBranch { name; _ } -> name
          | Ast.RuleTree { name; _ } -> name
        in
        (name, compile_rule reg id rule))
  in
  let rec compile_strategy = function
    | Ast.SCall name -> (
        match List.assoc_opt name compiled_rules with
        | Some rule -> Tableau.applyRule rule
        | None -> (
            let decl =
              List.find_opt
                (fun (s : Ast.strategy_decl) -> s.name = name)
                logic.strategies
            in
            match decl with
            | Some s -> compile_strategy s.body
            | None ->
                invalid_arg (Printf.sprintf "Unknown strategy or rule: %s" name)
            ))
    | Ast.SBang name -> (
        match List.assoc_opt name compiled_rules with
        | Some rule -> Tableau.applyRuleBang rule
        | None -> invalid_arg (Printf.sprintf "Unknown rule: %s" name))
    | Ast.SOrElse (s1, s2) ->
        Tableau.orElse (compile_strategy s1) (compile_strategy s2)
    | Ast.SAndThen (s1, s2) ->
        Tableau.andThen (compile_strategy s1) (compile_strategy s2)
    | Ast.SOrAlt (s1, s2) ->
        Tableau.orAlt (compile_strategy s1) (compile_strategy s2)
    | Ast.SAndAlt (s1, s2) ->
        Tableau.andAlt (compile_strategy s1) (compile_strategy s2)
    | Ast.SRepeat s -> Tableau.repeat (compile_strategy s)
  in
  let initial_branch =
    problem.formulas |> List.mapi (fun id expr -> (id, term_of_expr expr))
  in
  let initial_state : Tableau.proof_state =
    {
      tree = [ initial_branch ];
      next_fresh = 0;
      next_symbol = 0;
      next_formula_id = List.length problem.formulas;
      applied = [];
    }
  in
  (initial_state, compile_strategy logic.entry_strategy.body)
