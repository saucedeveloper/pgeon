module IntMap = Map.Make (struct
  type t = int
  let compare = compare
end)

module StringMap = Map.Make (struct
  type t = string
  let compare = String.compare
end)

type compiled_generator_call = {
  name : string;
  args : Term.t list;
}

type compiled_subst_rhs =
  | CG_Gen of compiled_generator_call
  | CG_Expr of compiled_where_expr

and compiled_subst = {
  target : string;
  target_index : int;
  rhs : compiled_subst_rhs;
}

and compiled_where_expr = {
  base : Term.t;
  substs : compiled_subst list;
}

and compiled_where_binding = {
  var : Term.name;
  expr : compiled_where_expr;
}

type formula_rule = {
  rule_t : Ast.rule_type;
  input : Term.t list;
  output : Term.t list list;
  where_bindings : compiled_where_binding list;
}

type tree_rule = {
  rule_t : Ast.rule_type;
  branch_pattern : Term.t list;
  branch_tail : string option;
  tree_var : string;
  where_clause : Ast.where_binding list;
  rhs_tree : Ast.tree_expr;
  meta_index : int StringMap.t;
}

let symbol_counter = ref 0

let rec string_of_term = function
  | Term.Bvar i -> Printf.sprintf "B%d" i
  | Term.Fvar i -> Printf.sprintf "F%d" i
  | Term.Mvar i -> Printf.sprintf "M%d" i
  | Term.App (fn, args) ->
      let args =
        match args with
        | [] -> ""
        | _ ->
            let inner = List.map string_of_term args |> String.concat ", " in
            Printf.sprintf "(%s)" inner
      in
      Printf.sprintf "f%d%s" fn args
  | Term.Bind (bn, t) ->
      Printf.sprintf "bind%d(%s)" bn (string_of_term t)

let string_of_term_list terms =
  String.concat "; " (List.map string_of_term terms)

let split_branch_tail exprs =
  match List.rev exprs with
  | Ast.LVar name :: rest -> (List.rev rest, Some name)
  | _ -> (exprs, None)

let rec extract_tree_pattern = function
  | Ast.TreeUnion (Ast.TreeBranch exprs, Ast.TreeLeaf (Ast.LVar name)) ->
      let head, tail = split_branch_tail exprs in
      Some (head, tail, name)
  | Ast.TreeUnion (Ast.TreeLeaf (Ast.LVar name), Ast.TreeBranch exprs) ->
      let head, tail = split_branch_tail exprs in
      Some (head, tail, name)
  | Ast.TreeUnion (a, b) -> (
      match extract_tree_pattern a with
      | Some _ as res -> res
      | None -> extract_tree_pattern b)
  | _ -> None

type rule_def =
  | FormulaRule of formula_rule
  | TreeRule of tree_rule

type match_result = { sigma : (Term.name * Term.t) list; inputs : int list }
type match_generator = match_result Term.generator

type formula_pending = {
  rule_index : int;
  branch : int list;
  branch_index : int;
  match_gen : match_generator;
  has_produced : bool;
}

type tree_pending = {
  rule_index : int;
  branch_indices : int list;
  sigma_gen : (Term.name * Term.t) list Term.generator;
  tail_terms : Term.t list;
}

type pending =
  | PendingFormula of formula_pending
  | PendingTree of tree_pending

type frame = {
  tree : Tree.t;
  formulas : Term.t list;
  fvars : string list;
  funcs : string list;
  binds : string list;
  strategy : Strategy.t list;
  next_fvar : int;
  pending : pending option;
}

type t = {
  frames : frame list;
  rules : rule_def list;
  strategy_env : (string * Strategy.t) list;
}

let init (ast : Ast.t) ~(fvars : string list) ~(funcs : string list)
    (problem : Term.t list) : t =
  let binds = Ast.symbol_bind ast in
  let meta_index =
    List.mapi (fun idx name -> (name, idx)) fvars
    |> List.fold_left
         (fun map (name, idx) -> StringMap.add name idx map)
         StringMap.empty
  in
  let rec compile_where_expr rule_name meta_env_map
      (wexpr : Ast.where_expr) =
    let base =
      match wexpr.base with
      | Ast.WExpr e -> Ast.term_of_expr fvars funcs binds e
      | Ast.WTree _ ->
          Log.error
            "[rule:where] status=error reason=tree_expression_unsupported \
             rule=%s\n"
            rule_name;
          exit 1
    in
    let binder_env =
      match wexpr.substs with
      | Some (Ast.SubstEntries entries) when entries <> [] -> (
          match wexpr.base with
          | Ast.WExpr (Ast.LVar name) when Ast.is_meta name -> (
              match StringMap.find_opt name meta_env_map with
              | Some env -> Some env
              | None ->
                  Log.error
                    "[rule:where] status=error reason=unknown_meta_env \
                     rule=%s meta=%s\n"
                    rule_name name;
                  exit 1)
          | _ ->
              Log.error
                "[rule:where] status=error reason=invalid_subst_base rule=%s\n"
                rule_name;
              exit 1)
      | _ -> None
    in
    let substs =
      match wexpr.substs with
      | None -> []
      | Some (Ast.SubstEntries entries) ->
          List.map
            (compile_subst rule_name meta_env_map binder_env)
            entries
      | Some (Ast.SubstRef _) ->
          Log.error
            "[rule:where] status=error reason=substitution_reference_unsupported \
             rule=%s\n"
            rule_name;
          exit 1
    in
    { base; substs }
  and compile_subst rule_name meta_env_map binder_env
      (subst : Ast.subst_entry) =
    let target_index =
      match binder_env with
      | Some env -> (
          match List.find_index (( = ) subst.target) env with
          | Some idx -> idx
          | None ->
              Log.error
                "[rule:where] status=error reason=unknown_binder rule=%s \
                 binder=%s\n"
                rule_name subst.target;
              exit 1)
      | None ->
          Log.error
            "[rule:where] status=error reason=missing_env rule=%s binder=%s\n"
            rule_name subst.target;
          exit 1
    in
    {
      target = subst.target;
      target_index;
      rhs = compile_subst_rhs rule_name meta_env_map subst.rhs;
    }
  and compile_subst_rhs rule_name meta_env_map = function
    | Ast.SR_Gen call ->
        let args = List.map (Ast.term_of_expr fvars funcs binds) call.gen_args in
        CG_Gen { name = call.gen_name; args }
    | Ast.SR_Expr expr ->
        CG_Expr (compile_where_expr rule_name meta_env_map expr)
  in
  let rules =
    List.map
      (fun (rd : Ast.rule_decl) ->
        match rd.tree_rule with
        | Some tree ->
            let branch_exprs, branch_tail, tree_var =
              match extract_tree_pattern tree.lhs_tree with
              | Some (exprs, tail, name) -> (exprs, tail, name)
              | None ->
                  Log.error
                    "[rule:tree] status=error reason=unsupported_pattern \
                     rule=%s\n"
                    rd.name;
                  exit 1
            in
            let branch_pattern =
              List.map (Ast.term_of_expr fvars funcs binds) branch_exprs
            in
            TreeRule
              {
                rule_t = rd.arrow;
                branch_pattern;
                branch_tail;
                tree_var;
                where_clause = rd.where_clause;
                rhs_tree = tree.rhs_tree;
                meta_index;
              }
        | None ->
            let rule_name = rd.name in
            let input = List.map (Ast.term_of_expr fvars funcs binds) rd.lhs in
            let output =
              List.map (List.map (Ast.term_of_expr fvars funcs binds)) rd.rhs
            in
            let meta_env_map =
              List.fold_left
                (fun map (name, env) -> StringMap.add name env map)
                StringMap.empty rd.meta_envs
            in
            let where_bindings =
              List.map
                (fun (binding : Ast.where_binding) ->
                  let var =
                    match StringMap.find_opt binding.var meta_index with
                    | Some idx -> idx
                    | None ->
                        Log.error
                          "[rule:where] status=error reason=unknown_meta \
                           rule=%s meta=%s\n"
                          rule_name binding.var;
                        exit 1
                  in
                  let expr =
                    compile_where_expr rule_name meta_env_map binding.value
                  in
                  { var; expr })
                rd.where_clause
            in
            FormulaRule
              { rule_t = rd.arrow; input; output; where_bindings })
      ast.rules
  in
  let strategy_env, main_strategy = Ast.compile_strategies ast in
  {
    frames =
      [
        {
          tree = Tree.init problem;
          formulas = problem;
          fvars;
          funcs;
          binds;
          strategy = [ main_strategy ];
          next_fvar = 0;
          pending = None;
        };
      ];
    rules;
    strategy_env;
  }
  |> fun tbl ->
  List.iteri
    (fun idx rule ->
      match rule with
      | FormulaRule fr ->
          let inputs = string_of_term_list fr.input in
          Log.debug "[rule:init] index=%d kind=formula inputs=[%s]\n" idx inputs
      | TreeRule tr ->
          Log.debug
            "[rule:init] index=%d kind=tree branch_size=%d tree_var=%s \
             pattern=[%s] tail=%s\n"
            idx (List.length tr.branch_pattern) tr.tree_var
            (string_of_term_list tr.branch_pattern)
            (match tr.branch_tail with Some v -> v | None -> "-"))
    tbl.rules;
  tbl

let join_map sep f lst = String.concat sep (List.map f lst)

let perm_n lst k =
  let n = List.length lst in
  let arr = Array.of_list lst in
  let used = Array.make n false in
  let curr = Array.make k (Obj.magic 0) in
  let results = ref [] in
  let rec aux depth =
    if depth = k then results := Array.to_list curr :: !results
    else
      for i = 0 to n - 1 do
        if not used.(i) then (
          used.(i) <- true;
          curr.(depth) <- arr.(i);
          aux (depth + 1);
          used.(i) <- false)
      done
  in
  aux 0;
  !results

let rec take n lst =
  if n <= 0 then []
  else
    match lst with
    | [] -> []
    | hd :: tl -> hd :: take (n - 1) tl

let rec drop n lst =
  if n <= 0 then lst
  else
    match lst with
    | [] -> []
    | _ :: tl -> drop (n - 1) tl

type branch_context = { tree : Tree.t; branch : int list; anchor : int }

let branch_index branch =
  match List.rev branch with [] -> None | idx :: _ -> Some idx

let push_formulas tree formulas branch new_formulas node =
  let branch_terms = List.map (fun idx -> List.nth formulas idx) branch in
  let dedup_branch existing forms =
    let rec aux seen acc = function
      | [] -> List.rev acc
      | t :: tl ->
          if List.exists (fun existing -> Term.equal existing t) seen then
            aux seen acc tl
          else aux (t :: seen) (t :: acc) tl
    in
    aux existing [] forms
  in
  let filtered =
    List.filter_map
      (fun forms ->
        let deduped = dedup_branch branch_terms forms in
        if deduped = [] then None else Some deduped)
      new_formulas
  in
  if filtered = [] then (
    Log.debug "[rule:push] status=skip reason=dedup branch_node=%d\n" node;
    None)
  else
    let flat_new = List.concat filtered in
    let base = List.length formulas in
    let new_forms = formulas @ flat_new in
    let branch_ids_rev, _ =
      List.fold_left
        (fun (acc, id) branch_forms ->
          let len = List.length branch_forms in
          let ids = List.init len (fun i -> id + i) in
          (ids :: acc, id + len))
        ([], base) filtered
    in
    let branch_ids = List.rev branch_ids_rev in

    let rec make_path = function
      | [] -> failwith "push_formula: new_formulas is empty"
      | [ i ] -> { Tree.node = i; childs = Some [] }
      | i :: rest -> { node = i; childs = Some [ make_path rest ] }
    in
    let new_subtree = List.map make_path branch_ids in

    let rec update t =
      if t.Tree.node = node then
        let new_childs =
          match t.childs with
          | None ->
              failwith "push_formula: tries to add formulas to a closed branch"
          | Some olds -> Some (olds @ new_subtree)
        in
        { t with childs = new_childs }
      else
        match t.childs with
        | None -> t
        | Some chs ->
            let chs = List.map update chs in
            { t with childs = Some chs }
    in
    let new_tree = update tree in
    Log.debug
      "[rule:push] status=ok branch_node=%d new_formula_count=%d \
       new_branch_count=%d\n"
      node (List.length flat_new) (List.length filtered);
    Some (new_tree, new_forms)

let instantiate_outputs sigma outputs =
  List.map (List.map (Term.substitute sigma)) outputs

let env_of_sigma sigma =
  List.fold_left (fun map (name, term) -> IntMap.add name term map) IntMap.empty
    sigma

let env_bindings env = IntMap.bindings env

let eval_generator ctx env call =
  let env_assoc = env_bindings env in
  let args_terms =
    List.map (fun term -> Term.substitute env_assoc term) call.args
  in
  let gargs =
    List.map (fun term -> Generator.V_string (string_of_term term)) args_terms
  in
  match Generator.eval call.name ctx gargs with
  | None -> None
  | Some None ->
      Log.error
        "[generator] status=error reason=call_failed name=%s\n" call.name;
      None
  | Some (Some term) -> Some term

let rec eval_where_expr env ctx expr =
  let env_assoc = env_bindings env in
  let base = Term.substitute env_assoc expr.base in
  List.fold_left
    (fun acc subst ->
      match acc with
      | None -> None
      | Some term -> (
          match eval_subst_rhs env ctx subst.rhs with
          | None -> None
          | Some rhs -> Some (Term.subst_bvar term subst.target_index rhs)))
    (Some base) expr.substs

and eval_subst_rhs env ctx = function
  | CG_Gen call -> eval_generator ctx env call
  | CG_Expr expr -> eval_where_expr env ctx expr

let eval_where_bindings env ctx bindings =
  List.fold_left
    (fun acc binding ->
      match acc with
      | None -> None
      | Some env -> (
          match eval_where_expr env ctx binding.expr with
          | None -> None
          | Some term -> Some (IntMap.add binding.var term env)))
    (Some env) bindings

let find_match formulas branch pattern =
  let arity = List.length pattern in
  let perms = ref (perm_n branch arity) in
  let current : (int list * ( (Term.name * Term.t) list) Term.generator) option ref =
    ref None
  in
  let rec next () =
    match !current with
    | Some (perm, gen) -> (
        match gen () with
        | Some sigma -> Some { sigma; inputs = perm }
        | None ->
            current := None;
            next ())
    | None -> (
        match !perms with
        | [] -> None
        | perm :: rest ->
            perms := rest;
            let candidates = List.map (List.nth formulas) perm in
            Log.debug "[rule:match:try] candidates=[%s] pattern=[%s]\n"
              (String.concat ", " (List.map string_of_term candidates))
              (String.concat ", " (List.map string_of_term pattern));
            let gen = Term.rule_match_gen candidates pattern in
            current := Some (perm, gen);
            next ())
  in
  next

let term_in_branch formulas branch term =
  List.exists
    (fun idx ->
      let existing = List.nth formulas idx in
      Term.equal existing term)
    branch

let has_new_formula formulas branch outputs =
  List.exists
    (fun branch_output ->
      List.exists
        (fun term -> not (term_in_branch formulas branch term))
        branch_output)
    outputs

let index_in_branch branch node =
  let rec aux depth = function
    | [] -> None
    | hd :: tl -> if hd = node then Some depth else aux (depth + 1) tl
  in
  aux 0 branch

let removal_order branch inputs =
  let depth_pairs =
    List.filter_map
      (fun node ->
        match index_in_branch branch node with
        | None -> None
        | Some depth -> Some (depth, node))
      inputs
  in
  let sorted = List.sort (fun (d1, _) (d2, _) -> compare d2 d1) depth_pairs in
  let rec dedup seen = function
    | [] -> []
    | (_, node) :: tl ->
        if List.mem node seen then dedup seen tl
        else node :: dedup (node :: seen) tl
  in
  dedup [] sorted

let merge_children child siblings =
  match (child.Tree.childs, siblings) with
  | None, [] -> None
  | None, _ -> Some siblings
  | Some grandchildren, _ -> Some (grandchildren @ siblings)

let remove_root tree target =
  if tree.Tree.node <> target then Some tree
  else
    match tree.childs with
    | None -> None
    | Some [] -> None
    | Some (child :: siblings) ->
        Some { child with childs = merge_children child siblings }

let rec remove_under_parent tree parent target =
  if tree.Tree.node = parent then
    match tree.childs with
    | None -> Some tree
    | Some childs ->
        let rec rebuild acc = function
          | [] -> Some { tree with childs = Some (List.rev acc) }
          | child :: tl when child.Tree.node = target ->
              let promoted =
                match child.childs with None -> tl | Some rep -> rep @ tl
              in
              Some { tree with childs = Some (List.rev_append acc promoted) }
          | child :: tl -> (
              match remove_under_parent child parent target with
              | None -> None
              | Some child' -> rebuild (child' :: acc) tl)
        in
        rebuild [] childs
  else
    match tree.childs with
    | None -> Some tree
    | Some childs ->
        let rec process acc = function
          | [] -> Some { tree with childs = Some (List.rev acc) }
          | child :: tl -> (
              match remove_under_parent child parent target with
              | None -> None
              | Some child' -> process (child' :: acc) tl)
        in
        process [] childs

let remove_inputs tree branch anchor inputs =
  let order = removal_order branch inputs in
  let removed_anchor = List.exists (fun node -> node = anchor) order in
  let rec apply_removals tree branch = function
    | [] ->
        let anchor =
          if removed_anchor then branch_index branch else Some anchor
        in
        Option.map (fun anchor -> { tree; branch; anchor }) anchor
    | target :: tl -> (
        match index_in_branch branch target with
        | None -> apply_removals tree branch tl
        | Some depth -> (
            let parent =
              if depth = 0 then None else Some (List.nth branch (depth - 1))
            in
            let tree_opt =
              match parent with
              | None -> remove_root tree target
              | Some parent -> remove_under_parent tree parent target
            in
            match tree_opt with
            | None -> None
            | Some tree' ->
                let branch' = List.filter (fun node -> node <> target) branch in
                apply_removals tree' branch' tl))
  in
  apply_removals tree branch order

let ensure_formula_pending frame rule_index rule =
  match frame.pending with
  | Some (PendingFormula pending) when pending.rule_index = rule_index ->
      (frame, pending)
  | _ ->
      let tree = frame.tree in
      let branch, branch_index =
        match Tree.find_leftmost_branch tree with
        | None ->
            Log.error "apply_rule: no open branches";
            exit 1
        | Some (branch, index) -> (branch, index)
      in
      let pending =
        {
          rule_index;
          branch;
          branch_index;
          match_gen = find_match frame.formulas branch rule.input;
          has_produced = false;
        }
      in
      let frame = { frame with pending = Some (PendingFormula pending) } in
      (frame, pending)

let execute_formula_match
    (frame : frame)
    (rule : formula_rule)
    rest_strategy
    (pending : formula_pending)
    { sigma; inputs } =
  let tree = frame.tree in
  let formulas = frame.formulas in
  let branch = pending.branch in
  let branch_index = pending.branch_index in
  match rule.rule_t with
  | Close ->
      Log.debug "[rule:close] anchor=%d status=success\n" branch_index;
      [
        {
          frame with
          tree = Tree.close tree branch_index;
          strategy = rest_strategy;
          pending = None;
        };
      ]
  | rule_kind ->
      let matched_inputs = List.map (List.nth formulas) inputs in
      let env = env_of_sigma sigma in
      let next_fvar = ref frame.next_fvar in
      let make_fvar () =
        let id = !next_fvar in
        incr next_fvar;
        Term.Fvar id
      in
      let metas_map =
        List.fold_left
          (fun acc (k, v) -> Generator.IntMap.add k v acc)
          Generator.IntMap.empty (IntMap.bindings env)
      in
      let ctx : Generator.ctx =
        {
          inputs = matched_inputs;
          env =
            {
              metas = metas_map;
              fvars = Generator.IntMap.empty;
              scope = [];
              named = Generator.StringMap.empty;
            };
          make_fvar;
          make_symbol =
            (fun ~arity args ->
              let id = !symbol_counter in
              incr symbol_counter;
              Term.App (Hashtbl.hash (arity, id), args));
        }
      in
      match eval_where_bindings env ctx rule.where_bindings with
      | None ->
          Log.debug
            "[rule:apply] status=abort reason=where_failed anchor=%d\n"
            branch_index;
          []
      | Some env_result ->
          let sigma_all = env_bindings env_result in
          let output_instance = instantiate_outputs sigma_all rule.output in
          let branch_ctx =
            match rule_kind with
            | Invertible -> remove_inputs tree branch branch_index inputs
            | NoInvertible -> Some { tree; branch; anchor = branch_index }
            | Close -> None
          in
          match branch_ctx with
          | None ->
              Log.debug
                "[rule:apply] status=abort reason=prune_failed anchor=%d \
                 inputs=[%s]\n"
                branch_index
                (join_map "," string_of_int inputs);
              []
          | Some ctx_branch ->
              if
                not
                  (has_new_formula formulas ctx_branch.branch output_instance)
              then (
                Log.debug
                  "[rule:apply] status=skip reason=no_new_formula anchor=%d\n"
                  ctx_branch.anchor;
                [])
              else
                match
                  push_formulas ctx_branch.tree formulas ctx_branch.branch
                    output_instance ctx_branch.anchor
                with
                | None -> []
                | Some (new_tree, new_formulas) ->
                    let new_frame =
                      {
                        frame with
                        tree = new_tree;
                        formulas = new_formulas;
                        next_fvar = !next_fvar;
                        strategy = rest_strategy;
                        pending = None;
                      }
                    in
                    [ new_frame ]

let step_formula_rule frame rule_index rule rest_strategy =
  let frame, pending = ensure_formula_pending frame rule_index rule in
  match pending.match_gen () with
  | None ->
      if not pending.has_produced then Log.debug "[rule:match] status=miss\n";
      ([], None)
  | Some match_res ->
      let frames =
        execute_formula_match frame rule rest_strategy pending match_res
      in
      let pending =
        { pending with has_produced = true }
      in
      let resume_frame =
        { frame with pending = Some (PendingFormula pending) }
      in
      (frames, Some resume_frame)

type eval_tree =
  | EvalTreeVar of string
  | EvalTreeBranch of Term.t list
  | EvalTreeUnion of eval_tree * eval_tree

type tree_where_state = {
  meta_subst : (Term.name * Term.t) list;
  fvar_subst : (Term.name * Term.t) list;
  named_substs : (Term.name * Term.t) list Generator.StringMap.t;
  named_trees : eval_tree Generator.StringMap.t;
}

let empty_tree_where_state ?(fvar_subst = [])
    ?(named_substs = Generator.StringMap.empty)
    ?(named_trees = Generator.StringMap.empty) sigma =
  { meta_subst = sigma; fvar_subst; named_substs; named_trees }

let term_of_expr_in_frame frame expr =
  Ast.term_of_expr frame.fvars frame.funcs frame.binds expr

let rec eval_tree_expr frame state tree =
  match tree with
  | Ast.TreeLeaf (Ast.LVar name) -> EvalTreeVar name
  | Ast.TreeLeaf expr ->
      let term =
        term_of_expr_in_frame frame expr
        |> Term.substitute state.meta_subst |> Term.subst_fvar state.fvar_subst
      in
      EvalTreeBranch [ term ]
  | Ast.TreeBranch el ->
      let terms =
        List.map
          (fun expr ->
            term_of_expr_in_frame frame expr
            |> Term.substitute state.meta_subst |> Term.subst_fvar state.fvar_subst)
          el
      in
      EvalTreeBranch terms
  | Ast.TreeUnion (a, b) ->
      EvalTreeUnion (eval_tree_expr frame state a, eval_tree_expr frame state b)

let rec apply_tree_subst tree subs =
  match tree with
  | EvalTreeVar _ -> tree
  | EvalTreeBranch terms ->
      EvalTreeBranch (List.map (Term.subst_fvar subs) terms)
  | EvalTreeUnion (a, b) ->
      EvalTreeUnion (apply_tree_subst a subs, apply_tree_subst b subs)

let eval_tree_where_bindings frame tree_rule named_trees sigma =
  let open Ast in
  let state = ref (empty_tree_where_state ~named_trees sigma) in
  let error () =
    Log.debug "[tree:where] status=abort\n";
    None
  in
  let rec eval_bindings bindings =
    match bindings with
    | [] -> Some !state
    | binding :: tl -> (
        match binding.value.base with
      | Ast.WExpr (Ast.LFun ("unify", [ lhs; rhs ])) ->
            let lhs_term =
              term_of_expr_in_frame frame lhs
              |> Term.substitute !state.meta_subst
            in
            let rhs_term =
              term_of_expr_in_frame frame rhs
              |> Term.substitute !state.meta_subst
            in
            begin
              match Term.unify lhs_term rhs_term with
              | None -> error ()
              | Some subs -> (
                  match
                    (try Some (Term.compose_fvar_subst !state.fvar_subst subs)
                     with Invalid_argument _ -> None)
                  with
                  | None -> error ()
                  | Some fvar_subst ->
                      state :=
                        {
                          !state with
                          fvar_subst;
                          named_substs =
                            Generator.StringMap.add binding.var subs
                              !state.named_substs;
                        };
                      eval_bindings tl)
            end
        | Ast.WTree tree ->
            let evaluated = eval_tree_expr frame !state tree in
            let evaluated =
              match binding.value.substs with
              | Some (Ast.SubstRef (Ast.LVar name)) -> (
                  match Generator.StringMap.find_opt name !state.named_substs with
                  | Some subs -> apply_tree_subst evaluated subs
                  | None -> evaluated)
              | _ -> evaluated
            in
            state :=
              {
                !state with
                named_trees =
                  Generator.StringMap.add binding.var evaluated
                    !state.named_trees;
              };
            eval_bindings tl
        | Ast.WExpr expr -> (
            match StringMap.find_opt binding.var tree_rule.meta_index with
            | Some idx ->
                let term =
                  term_of_expr_in_frame frame expr
                  |> Term.substitute !state.meta_subst
                  |> Term.subst_fvar !state.fvar_subst
                in
                state :=
                  {
                    !state with
                    meta_subst = (idx, term) :: !state.meta_subst;
                  };
                eval_bindings tl
            | None ->
                Log.error
                  "[tree:where] status=error reason=unsupported_binding \
                   name=%s\n"
                  binding.var;
                error ()))
  in
  eval_bindings tree_rule.where_clause

let ensure_tree_pending frame rule_index rule =
  match frame.pending with
  | Some (PendingTree pending) when pending.rule_index = rule_index ->
      Some (frame, pending)
  | _ ->
      let tree = frame.tree in
      let branch_indices, _ =
        match Tree.find_leftmost_branch tree with
        | None ->
            Log.error "apply_tree_rule: no open branches";
            exit 1
        | Some (branch, index) -> (branch, index)
      in
      let branch_terms =
        List.rev (List.map (List.nth frame.formulas) branch_indices)
      in
      let head_len = List.length rule.branch_pattern in
      if List.length branch_terms < head_len then (
        Log.debug "[tree:match] status=miss reason=short_branch\n";
        None)
      else
        let prefix_terms = take head_len branch_terms in
        Log.debug "[tree:prefix] branch=[%s] prefix=[%s]\n"
          (String.concat ", " (List.map string_of_int branch_indices))
          (string_of_term_list prefix_terms);
        let pending =
          {
            rule_index;
            branch_indices;
            sigma_gen = Term.rule_match_gen prefix_terms rule.branch_pattern;
            tail_terms = drop head_len branch_terms;
          }
        in
        let frame = { frame with pending = Some (PendingTree pending) } in
        Some (frame, pending)

let execute_tree_match
    (frame : frame)
    (rule : tree_rule)
    rest_strategy
    (pending : tree_pending)
    sigma =
  let formulas = frame.formulas in
  let named_trees =
    let base =
      Generator.StringMap.add rule.tree_var (EvalTreeVar rule.tree_var)
        Generator.StringMap.empty
    in
    match rule.branch_tail with
    | Some tail_name ->
        Generator.StringMap.add tail_name (EvalTreeBranch pending.tail_terms) base
    | None -> base
  in
  match eval_tree_where_bindings frame rule named_trees sigma with
  | None -> []
  | Some state ->
      let new_formulas =
        List.map
          (fun term ->
            term |> Term.substitute state.meta_subst
            |> Term.subst_fvar state.fvar_subst)
          formulas
      in
      [
        {
          frame with
          formulas = new_formulas;
          strategy = rest_strategy;
          pending = None;
        };
      ]

let step_tree_rule frame rule_index rule rest_strategy =
  match ensure_tree_pending frame rule_index rule with
  | None -> ([], None)
  | Some (frame, pending) -> (
      match pending.sigma_gen () with
      | None -> ([], None)
      | Some sigma ->
          let frames = execute_tree_match frame rule rest_strategy pending sigma in
          let resume_frame =
            { frame with pending = Some (PendingTree pending) }
          in
          (frames, Some resume_frame))

let string_of_strategy s =
  let rec aux = function
    | Strategy.AndThen (s1, s2) -> Printf.sprintf "(%s ; %s)" (aux s1) (aux s2)
    | Strategy.OrElse (s1, s2) -> Printf.sprintf "(%s || %s)" (aux s1) (aux s2)
    | Strategy.Fail -> "FAIL"
    | Strategy.Repeat s -> Printf.sprintf "(%s)*" (aux s)
    | Strategy.Rule i -> string_of_int i
    | Strategy.Call name -> name
    | Strategy.Skip -> "SKIP"
  in
  let str = String.concat ",  " (List.map aux s) in
  Printf.sprintf "[%s]" str

let rec branch (o, c) = function
  | (t : Tree.t) -> (
      match t.childs with
      | None -> (o, c + 1)
      | Some [] -> (o + 1, c)
      | Some tl -> List.fold_left branch (o, c) tl)

let rec prove tableau =
  match tableau.frames with
  | [] -> Log.info "Failure\n"
  | frame :: frames -> (
      let o, _ = branch (0, 0) frame.tree in
      if o = 0 then (
        Log.info "Success\n";
        ())
      else
        match frame.strategy with
        | [] -> prove { tableau with frames }
        | Skip :: strategy ->
            prove { tableau with frames = { frame with strategy } :: frames }
        | Fail :: _ -> prove { tableau with frames }
        | Rule i :: strategy -> (
            match List.nth tableau.rules i with
            | FormulaRule rule ->
                let spawned, resume =
                  step_formula_rule frame i rule strategy
                in
                let frames_with_resume =
                  match resume with
                  | Some resume_frame -> resume_frame :: frames
                  | None -> frames
                in
                let updated_frames =
                  match spawned with
                  | [] -> frames_with_resume
                  | lst -> lst @ frames_with_resume
                in
                prove { tableau with frames = updated_frames }
            | TreeRule rule ->
                let spawned, resume =
                  step_tree_rule frame i rule strategy
                in
                let frames_with_resume =
                  match resume with
                  | Some resume_frame -> resume_frame :: frames
                  | None -> frames
                in
                let updated_frames =
                  match spawned with
                  | [] -> frames_with_resume
                  | lst -> lst @ frames_with_resume
                in
                prove { tableau with frames = updated_frames })
        | AndThen (s1, s2) :: strategy ->
            prove
              {
                tableau with
                frames =
                  { frame with strategy = s1 :: s2 :: strategy } :: frames;
              }
        | OrElse (s1, s2) :: strategy ->
            prove
              {
                tableau with
                frames =
                  { frame with strategy = s1 :: strategy }
                  :: { frame with strategy = s2 :: strategy }
                  :: frames;
              }
        | Repeat s :: strategy ->
            prove
              {
                tableau with
                frames =
                  {
                    frame with
                    strategy = OrElse (AndThen (s, Repeat s), Skip) :: strategy;
                  }
                  :: frames;
              }
        | Call name :: strategy -> (
            match List.assoc_opt name tableau.strategy_env with
            | Some body ->
                prove
                  {
                    tableau with
                    frames =
                      { frame with strategy = body :: strategy } :: frames;
                  }
            | None ->
                Log.error
                  "[strategy:call] status=error reason=unknown_strategy name=%s\n"
                  name;
                exit 1))
