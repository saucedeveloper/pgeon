type branch_template = { prefix : Term.t list; tail : Term.name option }

type tree_template = {
  branches : branch_template list;
  tail : Term.name option;
}

module StringSet = Set.Make (String)
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type rule_action_result = {
  nodes : Term.substitution;
  branch_tails : (Term.name * Term.t list) list;
  tree_tails : (Term.name * Term.t list list) list;
  fvars : Term.substitution;
}

type tree_action_state = {
  sigma : Term.substitution;
  branch_env : (Term.name * Term.t list) list;
  tree_env : (Term.name * Term.t list list) list;
  fvars : Term.substitution;
}

let rec string_of_term = function
  | Term.Bvar i -> Printf.sprintf "B%d" i
  | Term.Fvar i -> Printf.sprintf "F%d" i
  | Term.Mvar i -> Printf.sprintf "M%d" i
  | Term.App (name, args) ->
      let args =
        match args with
        | [] -> ""
        | _ ->
            let inner =
              args |> List.map string_of_term |> String.concat ", "
            in
            Printf.sprintf "(%s)" inner
      in
      Printf.sprintf "f%d%s" name args
  | Term.Bind (name, t) ->
      Printf.sprintf "bind%d(%s)" name (string_of_term t)

let string_of_term_list terms =
  terms |> List.map string_of_term |> String.concat "; "

let string_of_subst subst =
  let format prefix bindings =
    bindings
    |> List.map (fun (name, term) ->
           Printf.sprintf "%s%d -> %s" prefix name (string_of_term term))
    |> String.concat ", "
  in
  match subst with
  | Term.MetaSubstitution bindings -> format "M" bindings
  | Term.FreeVarSubstitution bindings -> format "F" bindings

let warned_free_unifier = ref false

let warn_free_unifier context =
  if not !warned_free_unifier then (
    warned_free_unifier := true;
    Log.warn "[tableau:%s] status=unsupported_unifier kind=free_var\n" context)

let term_to_string_with_names function_names binder_names term =
  let rec aux = function
    | Term.Bvar i -> Printf.sprintf "B%d" i
    | Term.Fvar i -> Printf.sprintf "F%d" i
    | Term.Mvar i -> Printf.sprintf "M%d" i
    | Term.App (idx, args) ->
        let name =
          if idx < List.length function_names then
            List.nth function_names idx
          else Printf.sprintf "f%d" idx
        in
        let args =
          match args with
          | [] -> ""
          | _ ->
              args |> List.map aux |> String.concat ", "
              |> Printf.sprintf "(%s)"
        in
        name ^ args
    | Term.Bind (idx, body) ->
        let name =
          if idx < List.length binder_names then
            List.nth binder_names idx
          else Printf.sprintf "bind%d" idx
        in
        Printf.sprintf "%s(%s)" name (aux body)
  in
  aux term

let tree_signature function_names binder_names tree =
  Tree.get_open_branches tree
  |> Utils.seq_to_list
  |> List.map (fun (_id, branch) ->
         branch
         |> List.map (term_to_string_with_names function_names binder_names)
         |> String.concat ",")
  |> List.sort String.compare
  |> String.concat "|"

let rule_cache_has cache idx signature =
  match IntMap.find_opt idx !cache with
  | Some set -> StringSet.mem signature set
  | None -> false

let rule_cache_mark cache idx signature =
  let updated =
    match IntMap.find_opt idx !cache with
    | Some set -> StringSet.add signature set
    | None -> StringSet.singleton signature
  in
  cache := IntMap.add idx updated !cache

let rules_fully_cached cache total signature =
  let rec aux idx =
    if idx >= total then true
    else if rule_cache_has cache idx signature then aux (idx + 1)
    else false
  in
  aux 0

let substitution_to_map subs =
  let bindings =
    match subs with
    | Term.MetaSubstitution map | Term.FreeVarSubstitution map -> map
  in
  List.fold_left
    (fun acc (name, term) -> Generator.IntMap.add name term acc)
    Generator.IntMap.empty bindings

let generator_scope_from_term term =
  let rec collect (bvars, fvars) = function
    | Term.Bvar i when i > 0 ->
        (IntSet.add i bvars, fvars)
    | Term.Bvar _ -> (bvars, fvars)
    | Term.Fvar i -> (bvars, IntSet.add i fvars)
    | Term.Mvar _ -> (bvars, fvars)
    | Term.App (_, args) ->
        List.fold_left collect (bvars, fvars) args
    | Term.Bind (_, body) -> collect (bvars, fvars) body
  in
  let bset, fset = collect (IntSet.empty, IntSet.empty) term in
  let fvars =
    fset |> IntSet.elements |> List.map (fun i -> Term.Fvar i)
  in
  let bvars =
    bset |> IntSet.elements |> List.map (fun i -> Term.Bvar (i - 1))
  in
  fvars @ bvars

let prepare_generator_ctx (ctx : Generator.ctx) sigma scope =
  let metas = substitution_to_map sigma in
  { ctx with env = { ctx.env with metas; scope } }

let () =
  Generator.register (module Generator.Fresh);
  Generator.register (module Generator.Cte);
  Generator.register (module Generator.Skolem);
  Generator.register (module Generator.Inst)

let rec strategy_signature = function
  | Strategy.Skip -> "S"
  | Strategy.Fail -> "F"
  | Strategy.Rule i -> "R" ^ string_of_int i
  | Strategy.Bang i -> "B" ^ string_of_int i
  | Strategy.Call s -> "C" ^ s
  | Strategy.AndThen (a, b) ->
      "A(" ^ strategy_signature a ^ "," ^ strategy_signature b ^ ")"
  | Strategy.OrElse (a, b) ->
      "O(" ^ strategy_signature a ^ "," ^ strategy_signature b ^ ")"
  | Strategy.Repeat s -> "P(" ^ strategy_signature s ^ ")"

type subst_template =
  | SubstNone
  | SubstGenerators of string list
  | SubstMge of Term.t * Term.t

type where_result_template =
  | ResultNode of Term.t
  | ResultBranch of branch_template
  | ResultTree of tree_template

and tree_where_binding = {
  target : Term.name;
  result : where_result_template;
  subst : subst_template;
}

let merge_substitutions base extras = Term.merge_meta_substitutions base extras

let update_env env key value = (key, value) :: List.remove_assoc key env

let apply_replacements replacements term =
  if replacements = [] then term else Term.var_open replacements term

let lookup_branch_tail env key =
  match List.assoc_opt key env with
  | Some value -> Some value
  | None ->
      Log.error
        "[tableau:tree_rule] status=error reason=unknown_branch_tail id=%d\n"
        key;
      None

let lookup_tree_tail env key =
  match List.assoc_opt key env with
  | Some value -> Some value
  | None ->
      Log.error
        "[tableau:tree_rule] status=error reason=unknown_tree_tail id=%d\n"
        key;
      None

let eval_branch_template_with_state state replacements template =
  let prefix =
    List.map
      (fun term ->
        let term' = Term.substitute state.sigma term in
        apply_replacements replacements term')
      template.prefix
  in
  match template.tail with
  | None -> Some prefix
  | Some tail_id -> (
      match lookup_branch_tail state.branch_env tail_id with
      | None -> None
      | Some tail ->
          let tail' = List.map (Term.substitute state.sigma) tail in
          Some (prefix @ tail'))

let eval_tree_template_with_state state replacements template =
  let rec eval_branches acc = function
    | [] -> Some (List.rev acc)
    | branch_template :: rest -> (
        match eval_branch_template_with_state state replacements branch_template with
        | None -> None
        | Some branch_terms -> eval_branches (branch_terms :: acc) rest)
  in
  match eval_branches [] template.branches with
  | None -> None
  | Some branches -> (
      match template.tail with
      | None -> Some branches
      | Some tail_id -> (
          match lookup_tree_tail state.tree_env tail_id with
          | None -> None
          | Some tail ->
              let tail' =
                List.map
                  (fun branch -> List.map (Term.substitute state.sigma) branch)
                  tail
              in
              Some (branches @ tail')))

let eval_tree_where_bindings (bindings : tree_where_binding list)
    (sigma : Term.substitution)
    (branch_tails : (Term.name * Term.t list) list)
    (tree_tails : (Term.name * Term.t list list) list)
    (ctx : Generator.ctx) =
  let evaluate_generators ctx names =
    let rec aux acc = function
      | [] -> Some (List.rev acc)
      | name :: rest -> (
          match Generator.eval name ctx [] with
          | Some term -> aux (term :: acc) rest
          | None ->
              Log.error
                "[tableau:tree_rule] status=error reason=generator_failed name=%s\n"
                name;
              None)
    in
    aux [] names
  in
  let rec aux state = function
    | [] ->
        Some
          {
            nodes = state.sigma;
            branch_tails = state.branch_env;
            tree_tails = state.tree_env;
            fvars = state.fvars;
          }
    | binding :: rest -> (
        match eval_binding state binding with
        | None -> None
        | Some state' -> aux state' rest)
  and eval_binding state binding =
    let process_subst state =
      match binding.subst with
      | SubstNone -> Some (state, [])
      | SubstGenerators generators -> (
          let scope =
            match binding.result with
            | ResultNode term ->
                Term.substitute state.sigma term |> generator_scope_from_term
            | _ -> []
          in
          Log.debug
            "[tableau:tree_where] target=%d generators=[%s] scope=[%s]\n"
            binding.target
            (String.concat "," generators)
            (string_of_term_list scope);
          let ctx_for_binding = prepare_generator_ctx ctx state.sigma scope in
          match evaluate_generators ctx_for_binding generators with
          | None -> None
          | Some replacements -> Some (state, replacements))
      | SubstMge (lhs, rhs) ->
          let lhs' = Term.substitute state.sigma lhs in
          let rhs' = Term.substitute state.sigma rhs in
          match Term.unify [ lhs' ] [ rhs' ] with
          | None -> None
          | Some (Term.MetaSubstitution theta) -> (
              match
                merge_substitutions state.sigma (Term.MetaSubstitution theta)
              with
              | None -> None
              | Some sigma' -> Some ({ state with sigma = sigma' }, []))
          | Some (Term.FreeVarSubstitution theta) -> (
              match
                Term.merge_free_substitutions state.fvars
                  (Term.FreeVarSubstitution theta)
              with
              | None -> None
              | Some fvars -> Some ({ state with fvars }, []))
    in
    match process_subst state with
    | None -> None
    | Some (state', replacements) -> (
        match binding.result with
        | ResultNode term ->
            let value =
              Term.substitute state'.sigma term |> apply_replacements replacements
            in
            merge_substitutions state'.sigma
              (Term.MetaSubstitution [ (binding.target, value) ])
            |> Option.map (fun sigma' -> { state' with sigma = sigma' })
        | ResultBranch template -> (
            match eval_branch_template_with_state state' replacements template with
            | None -> None
            | Some branch ->
                let branch_env =
                  update_env state'.branch_env binding.target branch
                in
                Some { state' with branch_env = branch_env })
        | ResultTree template -> (
            match eval_tree_template_with_state state' replacements template with
            | None -> None
            | Some tree ->
                let tree_env = update_env state'.tree_env binding.target tree in
                Some { state' with tree_env = tree_env }))
  in
  aux
    {
      sigma;
      branch_env = branch_tails;
      tree_env = tree_tails;
      fvars = Term.FreeVarSubstitution [];
    }
    bindings

type rule =
  | RuleClosure of {
      name : string;
      lhs : Term.t list;
    }
  | RuleBranch of {
      name : string;
      lhs : Term.t list;
      rhs : Term.t list list;
      is_invertible : bool;
      action :
        Term.substitution ->
        Generator.ctx ->
        (Term.substitution * Term.substitution option) option;
    }
  | RuleTree of {
      name : string;
      lhs : tree_template;
      rhs : tree_template;
      is_invertible : bool;
      where_clause : tree_where_binding list;
      action :
        Term.substitution ->
        (Term.name * Term.t list) list ->
        (Term.name * Term.t list list) list ->
        Generator.ctx ->
        rule_action_result option;
    }

let rule_name = function
  | RuleClosure { name; _ }
  | RuleBranch { name; _ }
  | RuleTree { name; _ } ->
      name

let remove_positions branch positions =
  let positions = positions |> List.sort_uniq compare in
  branch
  |> List.mapi (fun idx term -> (idx, term))
  |> List.filter_map (fun (idx, term) ->
         if List.mem idx positions then None else Some term)

let apply_free_substitution tree subst_opt =
  match subst_opt with
  | Some subst when not (Term.is_empty_substitution subst) ->
      Tree.map_formulas (Term.substitute subst) tree
  | _ -> tree

let rec list_equal eq a b =
  match (a, b) with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && list_equal eq xs ys
  | _ -> false

type proof_state = {
  tree : Tree.t;
  fvars_count : int;
  funcs_count : int;
  visited : StringSet.t ref;
  rule_cache : StringSet.t IntMap.t ref;
  fully_cached : StringSet.t ref;
  frame_cache : StringSet.t ref;
}

let build_generator_ctx (proof_state : proof_state) sigma inputs =
  let fvar_counter = ref proof_state.fvars_count in
  let func_counter = ref proof_state.funcs_count in
  let make_fvar () =
    let idx = !fvar_counter in
    incr fvar_counter;
    Term.Fvar idx
  in
  let make_symbol ~arity args =
    if List.length args <> arity then
      Log.warn
        "[tableau:generator] status=arity_mismatch expected=%d actual=%d\n"
        arity (List.length args);
    let idx = !func_counter in
    incr func_counter;
    Term.App (idx, args)
  in
  let ctx : Generator.ctx =
    {
      Generator.inputs = inputs;
      env =
        {
          Generator.metas = substitution_to_map sigma;
          fvars = Generator.IntMap.empty;
          scope = [];
          named = Generator.StringMap.empty;
        };
      make_fvar;
      make_symbol;
    }
  in
  let finalize () = (!fvar_counter, !func_counter) in
  (ctx, finalize)

let mark_state_fully_cached proof_state total signature =
  if rules_fully_cached proof_state.rule_cache total signature then
    proof_state.fully_cached :=
      StringSet.add signature !(proof_state.fully_cached)

type continuation =
  | KStrategy of Strategy.t
  | KRule of {
      rule_idx : int;
      signature : string;
      rule : rule;
      alternatives : proof_state Seq.t;
      next : Strategy.t;
    }

let continuation_item_signature = function
  | KStrategy strat -> "KS:" ^ strategy_signature strat
  | KRule { rule_idx; signature; next; _ } ->
      "KR:" ^ string_of_int rule_idx ^ ":" ^ signature ^ ":"
      ^ strategy_signature next

let continuation_signature continuation =
  continuation
  |> List.map continuation_item_signature
  |> String.concat ";"

type frame = { proof_state : proof_state; continuation : continuation list }

type tableau_data = {
  rules : rule list;
  strategy_env : (string * Strategy.t) list;
  function_names : string list;
  binder_names : string list;
}

type branch_where_binding_compiled =
  | WBGenerators of {
      target : Term.name;
      template : Term.t;
      generators : string list;
    }
  | WBUnify of {
      target : Term.name;
      template : Term.t;
      lhs : Term.t;
      rhs : Term.t;
    }

let compile_functions (functions : Ast2.function_decl list) =
  List.mapi
    (fun idx (f : Ast2.function_decl) ->
      Log.debug "[tableau:compile_functions] idx=%d name=%s\n" idx f.name;
      f.name)
    functions

let compile_binders (binders : Ast2.binder_decl list) =
  List.map (fun (b : Ast2.binder_decl) -> b.name) binders

let compile_formulas (functions : string list) (binders : string list)
    (formulas : Ast2.expr list) : Term.t list =
  let rec aux env = function
    | Ast2.EVar v -> (
        match List.find_index (( = ) v) env with
        | Some i -> Term.Bvar i
        | None ->
            Log.error
              "[tableau:compile_formulas] status=error reason=unknown_variable \
               %s\n"
              v;
            failwith "Unknown variable")
    | EApp (f, args) -> (
        match List.find_index (( = ) f) functions with
        | Some i -> Term.App (i, List.map (aux env) args)
        | None ->
            Log.error
              "[tableau:compile_formulas] status=error reason=unknown_function \
               %s\n"
              f;
            failwith "Unknown function")
    | EBind (b, var, body) -> (
        match List.find_index (( = ) b) binders with
        | Some i -> Term.Bind (i, aux (var :: env) body)
        | None ->
            Log.error
              "[tableau:compile_formulas] status=error reason=unknown_binder %s\n"
              b;
            failwith "Unknown binder")
    | EBranchTail name | ETreeTail name ->
        Log.error
          "[tableau:compile_formulas] status=error reason=unexpected_tail %s\n"
          name;
        failwith "Unexpected tail expression"
  in
  List.map (aux []) formulas

let compile_rules functions binders (_rules : Ast2.rule_decl list) : rule list =
let rec translate b_env m_env = function
  | Ast2.EVar x -> (
      match List.find_index (( = ) x) b_env with
      | Some k -> (Term.Bvar k, m_env)
      | None -> (
          match List.assoc_opt x m_env with
          | Some id -> (Mvar id, m_env)
          | None ->
              let id = List.length m_env in
              (Mvar id, (x, id) :: m_env)))
  | EApp (f, args) -> (
      match List.find_index (( = ) f) functions with
      | Some i ->
          let args', m_env' =
            List.fold_right
              (fun arg (acc, env) ->
                let arg', env' = translate b_env env arg in
                (arg' :: acc, env'))
            args ([], m_env)
          in
          (Term.App (i, args'), m_env')
      | None ->
            Log.error
              "[tableau:compile_rules] status=error reason=unknown_function %s\n"
              f;
            failwith "Unknown function")
  | EBind (b, x, body) -> (
      match List.find_index (( = ) b) binders with
      | Some i ->
          let body', m_env' = translate (x :: b_env) m_env body in
          (Term.Bind (i, body'), m_env')
      | None ->
          Log.error
            "[tableau:compile_rules] status=error reason=unknown_binder %s\n"
            b;
            failwith "Unknown binder")
    | EBranchTail name | ETreeTail name ->
        Log.error
          "[tableau:compile_rules] status=error reason=unexpected_tail %s\n"
          name;
        failwith "Unexpected tail expression"
  in
  let find_or_add env name =
    match List.assoc_opt name env with
    | Some id -> (id, env)
    | None ->
        let id = List.length env in
        (id, (name, id) :: env)
  in
  let has_branch_tail exprs =
    List.exists
      (function Ast2.EBranchTail _ -> true | _ -> false)
      exprs
  in
  let has_tree_tail exprs =
    List.exists (function Ast2.ETreeTail _ -> true | _ -> false) exprs
  in
  let compile_branch_template exprs m_env branch_env =
    let rec aux prefix tail m_env branch_env = function
      | [] ->
          ( { prefix = List.rev prefix; tail },
            m_env,
            branch_env )
      | Ast2.EBranchTail name :: rest ->
          if Option.is_some tail then (
            Log.error
              "[tableau:compile_rules] status=error reason=duplicate_branch_tail \
               %s\n"
              name;
            failwith "Duplicate branch tail");
          if rest <> [] then (
            Log.error
              "[tableau:compile_rules] status=error \
               reason=branch_tail_not_last %s\n"
              name;
            failwith "Branch tail must be last element");
          let id, branch_env = find_or_add branch_env name in
          aux prefix (Some id) m_env branch_env rest
      | Ast2.ETreeTail name :: _ ->
          Log.error
            "[tableau:compile_rules] status=error \
             reason=unexpected_tree_tail_in_branch %s\n"
            name;
          failwith "Unexpected tree tail inside branch pattern"
      | expr :: rest ->
          let term, m_env = translate [] m_env expr in
          aux (term :: prefix) tail m_env branch_env rest
    in
    aux [] None m_env branch_env exprs
  in
  let compile_tree_template branches m_env branch_env tree_env =
    let rec aux acc tree_tail m_env branch_env tree_env = function
      | [] ->
          ( { branches = List.rev acc; tail = tree_tail },
            m_env,
            branch_env,
            tree_env )
      | branch :: rest -> (
          match branch with
          | [ Ast2.ETreeTail name ] ->
              if Option.is_some tree_tail then (
                Log.error
                  "[tableau:compile_rules] status=error \
                   reason=duplicate_tree_tail %s\n"
                  name;
                failwith "Duplicate tree tail");
              if rest <> [] then (
                Log.error
                  "[tableau:compile_rules] status=error \
                   reason=tree_tail_not_last %s\n"
                  name;
                failwith "Tree tail must be last branch in pattern");
              let id, tree_env = find_or_add tree_env name in
              ( { branches = List.rev acc; tail = Some id },
                m_env,
                branch_env,
                tree_env )
          | _ ->
          let branch_template, m_env, branch_env =
            compile_branch_template branch m_env branch_env
              in
              aux (branch_template :: acc) tree_tail m_env branch_env tree_env
                rest)
    in
    aux [] None m_env branch_env tree_env branches
  in
  let compile_where_result_template expr m_env branch_env tree_env =
    match expr with
    | [] ->
        Log.error
          "[tableau:compile_rules] status=error reason=empty_where_expression\n";
        failwith "Empty where expression"
    | [ branch ] -> (
        match branch with
        | [] ->
            Log.error
              "[tableau:compile_rules] status=error \
               reason=empty_where_branch\n";
            failwith "Empty where branch"
        | _ when has_tree_tail branch ->
            let template, m_env, branch_env, tree_env =
              compile_tree_template expr m_env branch_env tree_env
            in
            (ResultTree template, m_env, branch_env, tree_env)
        | _ when has_branch_tail branch || List.length branch > 1 ->
            let template, m_env, branch_env =
              compile_branch_template branch m_env branch_env
            in
            (ResultBranch template, m_env, branch_env, tree_env)
        | term :: _ ->
            let term, m_env = translate [] m_env term in
            (ResultNode term, m_env, branch_env, tree_env))
    | _ ->
        let template, m_env, branch_env, tree_env =
          compile_tree_template expr m_env branch_env tree_env
        in
        (ResultTree template, m_env, branch_env, tree_env)
  in
  let add_binding subs (k, v) = Term.meta_substitution_add subs (k, v) in
  let compile_expr env expr =
    let term, env' = translate [] env expr in
    if List.length env' > List.length env then (
      Log.error
        "[tableau:compile_rules] status=error \
         reason=unknown_meta_variable_in_where_expression\n";
      failwith "Unknown meta variable in where expression")
    else (term, env')
  in
  let compile_expr_list env exprs =
    List.fold_right
      (fun expr (acc, env) ->
        let term, env' = compile_expr env expr in
        (term :: acc, env'))
      exprs ([], env)
  in
  let compile_expr_list_list env branches =
    List.fold_right
      (fun branch (acc, env) ->
        let terms, env' = compile_expr_list env branch in
        (terms :: acc, env'))
      branches ([], env)
  in
  let lookup_meta env name =
    match List.assoc_opt name env with
    | Some id -> id
    | None ->
        Log.error
          "[tableau:compile_rules] status=error reason=unknown_meta_variable %s\n"
          name;
        failwith "Unknown meta variable in where clause"
  in
  let parse_generator_call raw =
    let len = String.length raw in
    let has_suffix suf =
      let suf_len = String.length suf in
      len >= suf_len
      && String.equal (String.sub raw (len - suf_len) suf_len) suf
    in
    if len >= 3 && raw.[0] = '@' && has_suffix "()" then
      String.sub raw 1 (len - 3)
    else (
      Log.error
        "[tableau:compile_rules] status=error \
         reason=unsupported_generator_call %s\n"
        raw;
      failwith "Unsupported generator call")
  in
  let compile_tree_where_binding m_env branch_env tree_env
      (binding : Ast2.where_binding) =
    let result, m_env, branch_env, tree_env =
      compile_where_result_template binding.value.expr m_env branch_env tree_env
    in
    let target, m_env, branch_env, tree_env =
      match result with
      | ResultNode _ ->
          let id, m_env = find_or_add m_env binding.var in
          (id, m_env, branch_env, tree_env)
      | ResultBranch _ ->
          let id, branch_env = find_or_add branch_env binding.var in
          (id, m_env, branch_env, tree_env)
      | ResultTree _ ->
          let id, tree_env = find_or_add tree_env binding.var in
          (id, m_env, branch_env, tree_env)
    in
    let subst =
      match binding.value.subst with
      | WSubstEntries entries ->
          let generators =
            List.map (fun (entry : Ast2.where_subst_entry) -> entry.gen) entries
            |> List.map parse_generator_call
          in
          if generators = [] then SubstNone else SubstGenerators generators
      | WSMge { var1; var2 } ->
          let lhs = Term.Mvar (lookup_meta m_env var1) in
          let rhs = Term.Mvar (lookup_meta m_env var2) in
          SubstMge (lhs, rhs)
    in
    ({ target; result; subst }, m_env, branch_env, tree_env)
  in
  let compile_branch_where_binding env (binding : Ast2.where_binding) =
    let target, env_with_target =
      match List.assoc_opt binding.var env with
      | Some id -> (id, env)
      | None ->
          let id = List.length env in
          (id, (binding.var, id) :: env)
    in
    let terms, env' =
      compile_expr_list_list env_with_target binding.value.expr
    in
    let template =
      match terms with
      | [ [ term ] ] -> term
      | _ ->
          Log.error
            "[tableau:compile_rules] status=error \
             reason=unsupported_where_expression_shape var=%s\n"
            binding.var;
          failwith "Unsupported where expression shape"
    in
    let binding_compiled, env'' =
      match binding.value.subst with
      | WSubstEntries entries ->
          let generators =
            List.map (fun (entry : Ast2.where_subst_entry) -> entry.gen) entries
            |> List.map parse_generator_call
          in
          (WBGenerators { target; template; generators }, env')
      | WSMge { var1; var2 } ->
          let lhs = Term.Mvar (lookup_meta env' var1) in
          let rhs = Term.Mvar (lookup_meta env' var2) in
          (WBUnify { target; template; lhs; rhs }, env')
    in
    (binding_compiled, env'')
  in
  let aux = function
    | Ast2.RuleClosure { name; lhs } ->
        let lhs, _ =
          List.fold_right
            (fun expr (acc, env) ->
              let term, env' = translate [] env expr in
              (term :: acc, env'))
            lhs ([], [])
        in
        RuleClosure { name; lhs }
    | RuleBranch { name; lhs; rhs; is_invertible; where_clause } ->
        let lhs, m_env =
          List.fold_right
            (fun expr (acc, env) ->
              let term, env' = translate [] env expr in
              (term :: acc, env'))
            lhs ([], [])
        in
        let rhs, m_env =
          List.fold_right
            (fun branch (acc, env) ->
              let branch_terms, env' =
                List.fold_right
                  (fun expr (b_acc, b_env) ->
                    let term, b_env' = translate [] b_env expr in
                    (term :: b_acc, b_env'))
                  branch ([], env)
              in
              (branch_terms :: acc, env'))
            rhs ([], m_env)
        in
        let where_bindings, _ =
          List.fold_left
            (fun (acc, env) binding ->
              let compiled, env' =
                compile_branch_where_binding env binding
              in
              (compiled :: acc, env'))
            ([], m_env) where_clause
        in
        let where_bindings = List.rev where_bindings in
        let evaluate_generators ctx generators =
          let rec aux acc = function
            | [] -> Some (List.rev acc)
            | name :: rest -> (
                match Generator.eval name ctx [] with
                | Some term -> aux (term :: acc) rest
                | None -> None)
          in
          aux [] generators
        in
        let rec apply_where sigma fvars ctx = function
          | [] -> Some (sigma, fvars)
          | WBGenerators { target; template; generators } :: rest -> (
              let instantiated = Term.substitute sigma template in
              let scope = generator_scope_from_term instantiated in
              Log.debug
                "[tableau:where] target=%d generators=[%s] scope=[%s]\n"
                target
                (String.concat "," generators)
                (string_of_term_list scope);
              let ctx_for_binding = prepare_generator_ctx ctx sigma scope in
              match evaluate_generators ctx_for_binding generators with
              | None -> None
              | Some replacements ->
                  let term = instantiated |> Term.var_open replacements in
                  let sigma' = add_binding sigma (target, term) in
                  let next_ctx = prepare_generator_ctx ctx sigma' [] in
                  apply_where sigma' fvars next_ctx rest)
          | WBUnify { target; template; lhs; rhs } :: rest -> (
              match
                Term.unify
                  [ Term.substitute sigma lhs ]
                  [ Term.substitute sigma rhs ]
              with
              | None -> None
              | Some (Term.MetaSubstitution theta) -> (
                  match merge_substitutions sigma (Term.MetaSubstitution theta) with
                  | None -> None
                  | Some sigma_with_theta ->
                      let term = Term.substitute sigma_with_theta template in
                      let sigma' = add_binding sigma_with_theta (target, term) in
                      let next_ctx = prepare_generator_ctx ctx sigma' [] in
                      apply_where sigma' fvars next_ctx rest)
              | Some (Term.FreeVarSubstitution theta) -> (
                  match
                    Term.merge_free_substitutions fvars
                      (Term.FreeVarSubstitution theta)
                  with
                  | None -> None
                  | Some fvars' ->
                      let next_ctx = prepare_generator_ctx ctx sigma [] in
                      apply_where sigma fvars' next_ctx rest))
        in
        let action sigma ctx =
          match
            apply_where sigma (Term.FreeVarSubstitution []) ctx where_bindings
          with
          | None -> None
          | Some (sigma', fvars) ->
              let fvars_opt =
                if Term.is_empty_substitution fvars then None else Some fvars
              in
              Some (sigma', fvars_opt)
        in
        RuleBranch { name; lhs; rhs; is_invertible; action }
    | RuleTree { name; lhs; rhs; is_invertible; where_clause } ->
        let lhs_template, m_env, branch_env, tree_env =
          compile_tree_template lhs [] [] []
        in
        let rhs_template, m_env, branch_env, tree_env =
          compile_tree_template rhs m_env branch_env tree_env
        in
        let where_bindings, _m_env, _branch_env, _tree_env =
          List.fold_left
            (fun (acc, m_env, branch_env, tree_env) binding ->
              let compiled, m_env, branch_env, tree_env =
                compile_tree_where_binding m_env branch_env tree_env binding
              in
              (compiled :: acc, m_env, branch_env, tree_env))
            ([], m_env, branch_env, tree_env) where_clause
        in
        let where_bindings = List.rev where_bindings in
        let action sigma branch_tails tree_tails ctx =
          eval_tree_where_bindings where_bindings sigma branch_tails tree_tails ctx
        in
        RuleTree
          {
            name;
            lhs = lhs_template;
            rhs = rhs_template;
            is_invertible;
            where_clause = where_bindings;
            action;
          }
  in
  List.map aux _rules

let compile_strategies (rules : Ast2.rule_decl list)
    (s : Ast2.strategy_decl list) =
  let rec aux = function
    | Ast2.SCall s -> (
        match
          List.find_index
            (fun (rule : Ast2.rule_decl) ->
              match rule with
              | RuleClosure rule -> rule.name = s
              | RuleBranch rule -> rule.name = s
              | RuleTree rule -> rule.name = s)
            rules
        with
        | Some i -> Strategy.Rule i
        | None -> Strategy.Call s)
    | Ast2.SRuleBang s -> (
        match
          List.find_index
            (fun (rule : Ast2.rule_decl) ->
              match rule with
              | RuleClosure rule -> rule.name = s
              | RuleBranch rule -> rule.name = s
              | RuleTree rule -> rule.name = s)
            rules
        with
        | Some i -> Strategy.Bang i
        | None ->
            Log.error
              "[tableau:compile_strategies] status=error \
               reason=unknown_rule_for_bang name=%s\n"
              s;
            failwith ("Unknown rule for ! operator: " ^ s))
    | SOr (a, b) -> OrElse (aux a, aux b)
    | SThen (a, b) -> AndThen (aux a, aux b)
    | SRepeat s -> Repeat (aux s)
    | STry s -> OrElse (aux s, Skip)
  in
  List.map (fun strat -> (strat.Ast2.name, aux strat.Ast2.strategy)) s

let init (_logic_file : Ast2.logic_file) (_problem_file : Ast2.problem_file) :
    tableau_data * frame =
  let functions = compile_functions _logic_file.functions in
  let binders = compile_binders _logic_file.binders in
  let rules = compile_rules functions binders _logic_file.rules in
  let strategy_env =
    compile_strategies _logic_file.rules _logic_file.strategies
  in
  let functions = functions @ compile_functions _problem_file.functions in
  let formulas = compile_formulas functions binders _problem_file.formulas in

  let tree =
    match formulas with
    | [] ->
        Log.error "[tableau:init] status=error reason=empty_problem\n";
        failwith "empty_problem"
    | _ -> Tree.init formulas
  in
  let signature = tree_signature functions binders tree in
  let proof_state =
    {
      tree;
      fvars_count = 0;
      funcs_count = List.length functions;
      visited = ref (StringSet.singleton signature);
      rule_cache = ref IntMap.empty;
      fully_cached = ref StringSet.empty;
      frame_cache = ref StringSet.empty;
    }
  in
  let continuation =
    match List.assoc_opt "prove" strategy_env with
    | Some strat -> [ KStrategy strat ]
    | None -> (
        match strategy_env with
        | [] -> []
        | (_, strat) :: _ -> [ KStrategy strat ])
  in
  ( { rules; strategy_env; function_names = functions; binder_names = binders },
    { proof_state; continuation } )

let apply_rule (t : tableau_data) (rule : rule) (proof_state : proof_state) :
    proof_state Seq.t =
  let term_to_string =
    term_to_string_with_names t.function_names t.binder_names
  in
  let terms_to_string terms =
    terms |> List.map term_to_string |> String.concat "; "
  in
  let register_state trace ?fvars_count ?funcs_count tree =
    let signature = tree_signature t.function_names t.binder_names tree in
    if StringSet.mem signature !(proof_state.visited) then (
      Log.info "[tableau:%s] state_seen\n" trace;
      None)
    else (
      proof_state.visited :=
        StringSet.add signature !(proof_state.visited);
      Log.debug "[tableau:%s] visited_size=%d signature=%s\n" trace
        (StringSet.cardinal !(proof_state.visited))
        signature;
      let new_fvars =
        match fvars_count with Some v -> v | None -> proof_state.fvars_count
      in
      let new_funcs =
        match funcs_count with Some v -> v | None -> proof_state.funcs_count
      in
      Some { proof_state with tree; fvars_count = new_fvars; funcs_count = new_funcs })
  in
  (* call Term.math rule, it return a (int list * Term.substitution) Seq.t, which we need to map to proof_state Seq.t. the int list is a list of index, i.e a branch, (we can ignore it). the subsitution is the sigma. we need to apply the sigma to the rhs *)
  (* match_rule takes 2 arguments: branch and pattern. to generate the next in the sequence we can call on another branch. *)
  (* we can then Seq.append all the sequences together to get the final sequence *)
  let current_rule_name = rule_name rule in
  Log.info "[tableau:apply_rule] rule=%s status=start\n" current_rule_name;
  match rule with
  | RuleClosure { name = _; lhs } -> (
      (* since it's a closure, we can just close all open branches that match the lhs *)
      let tree, success =
        Tree.close proof_state.tree (fun branch ->
            Term.match_rule branch lhs |> Seq.is_empty |> not)
      in
      match success with
      | true ->
          Log.info
            "[tableau:apply_rule] rule=%s status=success kind=closure\n"
            current_rule_name;
          register_state "apply_closure" tree |> Option.to_seq
      | false ->
          Log.info
            "[tableau:apply_rule] rule=%s status=fail kind=closure\n"
            current_rule_name;
          Seq.empty)
  | RuleBranch { name = _; lhs; rhs; is_invertible; action } ->
      Tree.get_open_branches proof_state.tree
      |> Seq.concat_map (fun (branch_id, branch_terms) ->
            Log.info
              "[tableau:apply_branch] rule=%s branch_id=%d branch=[%s] lhs=[%s]\n"
              current_rule_name branch_id
              (terms_to_string branch_terms)
              (terms_to_string lhs);
            let matches =
              Term.match_rule branch_terms lhs |> Utils.seq_to_list
            in
            Log.info
              "[tableau:apply_branch] rule=%s branch_id=%d match_count=%d\n"
              current_rule_name branch_id (List.length matches);
            List.to_seq matches
            |> Seq.filter_map (fun (positions, sigma) ->
                    Log.info
                      "[tableau:apply_branch] rule=%s branch_id=%d match \
                       positions=[%s] sigma=[%s]\n"
                      current_rule_name branch_id
                      (positions
                      |> List.map string_of_int
                      |> String.concat ", ")
                      (string_of_subst sigma);
                    let ctx, finalize_ctx =
                      build_generator_ctx proof_state sigma branch_terms
                    in
                    match action sigma ctx with
                    | None ->
                        Log.info
                          "[tableau:apply_branch] rule=%s branch_id=%d \
                           action=fail\n"
                          current_rule_name branch_id;
                        None
                    | Some (sigma', fvars_subst) ->
                        let new_fvars, new_funcs = finalize_ctx () in
                        Log.info
                          "[tableau:apply_branch] rule=%s branch_id=%d \
                           action_sigma=[%s]\n"
                          current_rule_name branch_id
                          (string_of_subst sigma');
                        let base_branch =
                          if is_invertible then
                            remove_positions branch_terms positions
                          else branch_terms
                        in
                        if is_invertible then
                          Log.info
                            "[tableau:apply_branch] rule=%s branch_id=%d \
                             invertible=true removed=%d\n"
                            current_rule_name branch_id
                            (List.length positions)
                        else
                          Log.info
                            "[tableau:apply_branch] rule=%s branch_id=%d \
                             invertible=false preserved_inputs\n"
                            current_rule_name branch_id;
                        let new_branches =
                          if rhs = [] then
                            if base_branch = [] then [] else [ base_branch ]
                          else
                            List.map
                              (fun rhs_branch ->
                                base_branch
                                @ List.map (Term.substitute sigma') rhs_branch)
                              rhs
                        in
                        let tree = Tree.remove proof_state.tree branch_id in
                        let tree =
                          List.fold_left
                            (fun acc branch ->
                              if branch = [] then acc
                              else Tree.add_branch acc branch)
                            tree new_branches
                        in
                        let tree = apply_free_substitution tree fvars_subst in
                        Log.info
                          "[tableau:apply_branch] rule=%s branch_id=%d \
                           produced=%d\n"
                          current_rule_name branch_id
                          (List.length new_branches);
                        register_state "apply_branch" ~fvars_count:new_fvars
                          ~funcs_count:new_funcs tree))
 | RuleTree { name = _; lhs; rhs; is_invertible = _; where_clause = _; action } ->
      Log.info "[tableau:apply_tree] rule=%s status=start\n" current_rule_name;
      let branches =
        Tree.get_open_branches proof_state.tree
        |> Seq.fold_left (fun acc (id, terms) -> (id, terms) :: acc) []
        |> List.rev
      in
      let match_branch_template (template : branch_template) branch_terms sigma =
        Term.match_rule branch_terms template.prefix
        |> Seq.filter_map (fun (positions, sigma_delta) ->
               match merge_substitutions sigma sigma_delta with
               | None -> None
               | Some sigma' ->
                   let position_set =
                     List.fold_left (fun acc idx -> IntSet.add idx acc) IntSet.empty positions
                   in
                   let remainder =
                     branch_terms
                     |> List.mapi (fun idx term -> (idx, term))
                     |> List.filter (fun (idx, _) -> not (IntSet.mem idx position_set))
                     |> List.map snd
                   in
                   let tail_binding =
                     match template.tail with
                     | None ->
                         if remainder = [] then Some None else None
                     | Some tail_id -> Some (Some (tail_id, remainder))
                   in
                   match tail_binding with
                   | None -> None
                   | Some binding -> Some (sigma', binding))
      in
      let rec match_templates templates sigma branch_env matched =
        match templates with
        | [] ->
            let unmatched =
              List.filter (fun (id, _) -> not (List.mem id matched)) branches
            in
            (match lhs.tail with
            | None ->
                if unmatched <> [] then Seq.empty
                else Seq.return (sigma, branch_env, [])
            | Some tail_id ->
                let tail_value = List.map snd unmatched in
                Seq.return (sigma, branch_env, [ (tail_id, tail_value) ]))
        | template :: rest ->
            branches
            |> List.to_seq
            |> Seq.filter (fun (branch_id, _) -> not (List.mem branch_id matched))
            |> Seq.flat_map (fun (branch_id, branch_terms) ->
                   match_branch_template template branch_terms sigma
                   |> Seq.flat_map (fun (sigma', tail_binding) ->
                          let branch_env' =
                            match tail_binding with
                            | None -> branch_env
                            | Some (tail_id, tail_terms) ->
                                update_env branch_env tail_id tail_terms
                          in
                          match_templates rest sigma' branch_env'
                            (branch_id :: matched)))
      in
      match_templates lhs.branches Term.(MetaSubstitution []) [] []
      |> Seq.filter_map (fun (sigma, branch_env, tree_env) ->
             let ctx, finalize_ctx = build_generator_ctx proof_state sigma [] in
             match action sigma branch_env tree_env ctx with
             | None ->
                 Log.info
                   "[tableau:apply_tree] rule=%s action=fail\n"
                   current_rule_name;
                 None
             | Some result ->
                 let new_fvars, new_funcs = finalize_ctx () in
                 let state =
                   {
                     sigma = result.nodes;
                    branch_env = result.branch_tails;
                    tree_env = result.tree_tails;
                    fvars = result.fvars;
                  }
                in
               (match eval_tree_template_with_state state [] rhs with
                | None -> None
                | Some new_branches ->
                    Log.info
                      "[tableau:apply_tree] rule=%s produced=%d\n"
                      current_rule_name
                      (List.length new_branches);
                    let build_tree =
                      let aux = function
                        | [] -> None
                        | first_branch :: rest -> (
                            if first_branch = [] then None
                            else
                              try
                                let initial = Tree.init first_branch in
                                let final =
                                  List.fold_left
                                    (fun acc branch ->
                                      match acc with
                                      | None -> None
                                      | Some tree ->
                                          if branch = [] then None
                                          else
                                            (try
                                               Some (Tree.add_branch tree branch)
                                             with Failure _ -> None))
                                    (Some initial) rest
                                in
                                final
                              with Failure _ -> None)
                      in
                      aux
                    in
                    match build_tree new_branches with
                    | None -> None
                    | Some tree ->
                        let tree =
                          apply_free_substitution tree
                            (if Term.is_empty_substitution result.fvars then None
                             else Some result.fvars)
                        in
                        register_state "apply_tree" ~fvars_count:new_fvars
                          ~funcs_count:new_funcs tree))

let branch_contains_terms branch terms =
  let rec aux branch = function
    | [] -> true
    | term :: rest -> (
        let rec remove = function
          | [] -> None
          | t :: tl when Term.equal t term -> Some tl
          | t :: tl ->
              remove tl |> Option.map (fun tail -> t :: tail)
        in
        match remove branch with
        | None -> false
        | Some branch' -> aux branch' rest)
  in
  aux branch terms

let find_branch_by_terms branches terms =
  List.find_opt
    (fun (_id, branch_terms) -> branch_contains_terms branch_terms terms)
    branches

let apply_rule_snapshot (t : tableau_data) (rule : rule)
    (proof_state : proof_state) : proof_state option =
  match rule with
  | RuleClosure { lhs; _ } -> (
      let tree, success =
        Tree.close proof_state.tree (fun branch ->
            Term.match_rule branch lhs |> Seq.is_empty |> not)
      in
      if success then (
        let signature =
          tree_signature t.function_names t.binder_names tree
        in
        proof_state.visited :=
          StringSet.add signature !(proof_state.visited);
        Some { proof_state with tree })
      else None)
  | RuleBranch
      { lhs; rhs; is_invertible; action; name = rule_name } ->
      let branches =
        Tree.get_open_branches proof_state.tree |> Utils.seq_to_list
      in
      let tasks =
        branches
        |> List.concat_map (fun (branch_id, branch_terms) ->
               Term.match_rule branch_terms lhs |> Utils.seq_to_list
               |> List.map (fun (positions, _sigma) ->
                      let matched_terms =
                        List.map (List.nth branch_terms) positions
                      in
                      (branch_id, matched_terms)))
      in
      let apply_task state (original_id, matched_terms) =
        let branches =
          Tree.get_open_branches state.tree |> Utils.seq_to_list
        in
        let branch_opt =
          match List.find_opt (fun (id, _) -> id = original_id) branches with
          | Some _ as found -> found
          | None -> find_branch_by_terms branches matched_terms
        in
        match branch_opt with
        | None -> None
        | Some (branch_id, branch_terms) -> (
            let matches =
              Term.match_rule branch_terms lhs |> Utils.seq_to_list
            in
            let match_opt =
              List.find_map
                (fun (positions, sigma) ->
                  let actual =
                    List.map (List.nth branch_terms) positions
                  in
                  if list_equal Term.equal actual matched_terms then
                    Some (positions, sigma)
                  else None)
                matches
            in
            match match_opt with
            | None -> None
            | Some (positions, sigma) ->
                let ctx, finalize_ctx =
                  build_generator_ctx state sigma branch_terms
                in
                match action sigma ctx with
                | None -> None
                | Some (sigma', fvars_subst) ->
                    let new_fvars, new_funcs = finalize_ctx () in
                    let base_branch =
                      if is_invertible then
                        remove_positions branch_terms positions
                      else branch_terms
                    in
                    let new_branches =
                      if rhs = [] then
                        if base_branch = [] then [] else [ base_branch ]
                      else
                        List.map
                          (fun rhs_branch ->
                            base_branch
                            @ List.map (Term.substitute sigma') rhs_branch)
                          rhs
                    in
                    let tree = Tree.remove state.tree branch_id in
                    let tree =
                      List.fold_left
                        (fun acc branch ->
                          if branch = [] then acc
                          else Tree.add_branch acc branch)
                        tree new_branches
                    in
                    let tree = apply_free_substitution tree fvars_subst in
                    let signature =
                      tree_signature t.function_names t.binder_names tree
                    in
                    state.visited :=
                      StringSet.add signature !(state.visited);
                    Some
                      {
                        state with
                        tree;
                        fvars_count = new_fvars;
                        funcs_count = new_funcs;
                      })
      in
      let applied_count, final_state =
        List.fold_left
          (fun (count, state) task ->
            match apply_task state task with
            | None -> (count, state)
            | Some state' -> (count + 1, state'))
          (0, proof_state) tasks
      in
      if applied_count > 0 then (
        Log.info
          "[tableau:bang] rule=%s applied=%d\n"
          rule_name applied_count;
        Some final_state)
      else None
  | RuleTree _ ->
      Log.warn
        "[tableau:bang] status=unsupported_kind rule=%s\n"
        (rule_name rule);
      None

let prove (logic_file : Ast2.logic_file) (problem_file : Ast2.problem_file) :
    bool =
  let tableau_data, initial_frame = init logic_file problem_file in
  let rec aux (frames : frame list) : bool =
    match frames with
    | [] ->
        Log.info "[tableau:prove] status=closed reason=no_frames\n";
        false
    | { proof_state; continuation } :: rest_frames -> (
        let state_signature =
          tree_signature tableau_data.function_names tableau_data.binder_names
            proof_state.tree
        in
        let frame_signature =
          state_signature ^ "|" ^ continuation_signature continuation
        in
        if StringSet.mem frame_signature !(proof_state.frame_cache) then (
          Log.debug
            "[tableau:prove] status=skip_frame_cache signature=%s\n"
            frame_signature;
          aux rest_frames)
        else (
          proof_state.frame_cache :=
            StringSet.add frame_signature !(proof_state.frame_cache);
          if StringSet.mem state_signature !(proof_state.fully_cached) then (
            Log.debug
              "[tableau:prove] status=skip_fully_cached signature=%s\n"
              state_signature;
            aux rest_frames)
          else
            match Tree.has_open_branches proof_state.tree with
            | false ->
                Log.info
                  "[tableau:prove] status=closed reason=no_open_branches\n";
                true
            | true -> (
                match continuation with
            | [] ->
                Log.info
                  "[tableau:prove] status=backtracked reason=no_continuation\n";
                aux rest_frames
            | KStrategy strat :: rest_cont -> (
                match strat with
                | Skip ->
                    aux
                      ({ proof_state; continuation = rest_cont } :: rest_frames)
                | Fail ->
                    Log.info
                      "[tableau:prove] status=backtracked reason=strategy_fail\n";
                    aux rest_frames
                | Rule i ->
                    if
                      rule_cache_has proof_state.rule_cache i state_signature
                    then (
                      Log.debug
                        "[tableau:strategy] status=cache_hit rule=%d\n" i;
                      let total_rules = List.length tableau_data.rules in
                      mark_state_fully_cached proof_state total_rules
                        state_signature;
                      aux rest_frames)
                    else
                      let rule = List.nth tableau_data.rules i in
                      Log.info
                        "[tableau:strategy] rule=%s idx=%d status=attempt \
                         signature=%s\n"
                        (rule_name rule) i state_signature;
                      let alternatives =
                        apply_rule tableau_data rule proof_state
                      in
                      aux
                        ({
                           proof_state;
                           continuation =
                             KRule
                               {
                                 rule_idx = i;
                                 signature = state_signature;
                                 rule;
                                 alternatives;
                                 next = Skip;
                               }
                             :: rest_cont;
                         }
                        :: rest_frames)
                | Strategy.Bang i ->
                    let rule = List.nth tableau_data.rules i in
                    Log.info
                      "[tableau:strategy] rule=%s idx=%d status=bang_attempt \
                       signature=%s\n"
                      (rule_name rule) i state_signature;
                    (match
                       apply_rule_snapshot tableau_data rule proof_state
                     with
                     | None ->
                         Log.info
                           "[tableau:strategy] rule=%s idx=%d status=bang_fail\n"
                           (rule_name rule) i;
                         aux rest_frames
                     | Some proof_state' ->
                         Log.info
                           "[tableau:strategy] rule=%s idx=%d \
                            status=bang_success\n"
                           (rule_name rule) i;
                         aux
                           ({
                              proof_state = proof_state';
                              continuation = rest_cont;
                            }
                           :: rest_frames))
                | Call s -> (
                    match List.assoc_opt s tableau_data.strategy_env with
                    | None ->
                        Log.error
                          "[tableau:prove] status=error reason=unknown_strategy\n";
                        false
                    | Some strat' ->
                        aux
                          ({
                             proof_state;
                             continuation = KStrategy strat' :: rest_cont;
                           }
                          :: rest_frames))
                | AndThen (s1, s2) ->
                    aux
                      ({
                         proof_state;
                         continuation =
                           KStrategy s1 :: KStrategy s2 :: rest_cont;
                       }
                      :: rest_frames)
                | OrElse (s1, s2) ->
                    let try_s1 =
                      { proof_state; continuation = KStrategy s1 :: rest_cont }
                    in
                    let try_s2 =
                      { proof_state; continuation = KStrategy s2 :: rest_cont }
                    in
                    aux (try_s1 :: try_s2 :: rest_frames)
                | Repeat s ->
                    let total_rules = List.length tableau_data.rules in
                    if
                      rules_fully_cached proof_state.rule_cache total_rules
                        state_signature
                    then (
                      mark_state_fully_cached proof_state total_rules
                        state_signature;
                      Log.debug
                        "[tableau:strategy] repeat_skip signature=%s\n"
                        state_signature;
                      aux ({ proof_state; continuation = rest_cont } :: rest_frames))
                    else
                      let once =
                        {
                          proof_state;
                          continuation =
                            KStrategy s :: KStrategy (Repeat s) :: rest_cont;
                        }
                      in
                      let stop = { proof_state; continuation = rest_cont } in
                      aux (once :: stop :: rest_frames))
            | KRule { rule_idx; signature; rule; alternatives; next }
              :: rest_cont -> (
                match alternatives () with
                | Seq.Nil ->
                    let total_rules = List.length tableau_data.rules in
                    rule_cache_mark proof_state.rule_cache rule_idx signature;
                    mark_state_fully_cached proof_state total_rules signature;
                    Log.info
                      "[tableau:rule] rule=%s idx=%d signature=%s status=fail\n"
                      (rule_name rule) rule_idx signature;
                    Log.info
                      "[tableau:prove] status=backtracked reason=no_alternatives\n";
                    aux rest_frames
                | Seq.Cons (alt, alternatives) ->
                    Log.info
                      "[tableau:rule] rule=%s idx=%d signature=%s status=success\n"
                      (rule_name rule) rule_idx signature;
                    let success =
                      {
                        proof_state = alt;
                        continuation = KStrategy next :: rest_cont;
                      }
                    in
                    let failure =
                      {
                        proof_state;
                        continuation =
                          KRule
                            { rule_idx; signature; rule; alternatives; next }
                          :: rest_cont;
                      }
                    in
                    aux (success :: failure :: frames)))
        ))
  in
  aux [ initial_frame ]
