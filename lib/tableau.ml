type rule_def = {
  rule_t : Ast.rule_type;
  input : Term.t list;
  output : Term.t list list;
}

type frame = {
  tree : Tree.t;
  formulas : Term.t list;
  fvars : string list;
  funcs : string list;
  strategy : Strategy.t list;
}

type t = { frames : frame list; rules : rule_def list }

let init (ast : Ast.t) ~(fvars : string list) ~(funcs : string list)
    (problem : Term.t list) : t =
  let binds = Ast.symbol_bind ast in
  let rules =
    List.map
      (fun (rd : Ast.rule_decl) ->
        {
          rule_t = rd.arrow;
          input = List.map (Ast.term_of_expr fvars funcs binds) rd.lhs;
          output =
            List.map (List.map (Ast.term_of_expr fvars funcs binds)) rd.rhs;
        })
      ast.rules
  in
  {
    frames =
      [
        {
          tree = Tree.init problem;
          formulas = problem;
          fvars;
          funcs;
          strategy = [ Ast.main_strategy ast ];
        };
      ];
    rules;
  }

let join_map sep f lst = String.concat sep (List.map f lst)

let string_of_term fvars funcs term =
  let rec string_of_term = function
    | Term.Bvar i -> string_of_int i
    | Term.Fvar i -> (List.nth fvars) i
    | Term.App (name, args) ->
        Printf.sprintf "%s(%s)" (List.nth funcs name)
          (join_map ", " string_of_term args)
    | Term.Bind (name, t) -> Printf.sprintf "%d. %s" name (string_of_term t)
  in
  string_of_term term

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

type match_result = { sigma : (Term.name * Term.t) list; inputs : int list }
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

let find_match formulas branch rule =
  let arity = List.length rule.input in
  let perms = perm_n branch arity in
  let match_for perm =
    let candidates = List.map (List.nth formulas) perm in
    match Term.p_match rule.input candidates with
    | None -> None
    | Some sigma -> Some { sigma; inputs = perm }
  in
  List.find_map match_for perms

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

let apply_rule fvars funcs (tree : Tree.t) (formulas : Term.t list)
    (rule : rule_def) =
  let string_of_term = string_of_term fvars funcs in
  let rule_inputs = join_map "," string_of_term rule.input in
  let rule_outputs =
    String.concat "|"
      (List.map (fun tl -> join_map "," string_of_term tl) rule.output)
  in
  Log.debug "[rule:select] inputs=[%s] outputs=[%s]\n" rule_inputs rule_outputs;
  let branch, branch_index =
    match Tree.find_leftmost_branch tree with
    | None ->
        Log.error "apply_rule: no open branches";
        exit 1
    | Some (branch, index) -> (branch, index)
  in
  let branch_terms =
    join_map "," (fun i -> string_of_term (List.nth formulas i)) branch
  in
  Log.debug "[rule:branch] anchor=%d terms=[%s]\n" branch_index branch_terms;
  match find_match formulas branch rule with
  | None ->
      Log.debug "[rule:match] status=miss\n";
      None
  | Some { sigma; inputs } -> (
      match rule.rule_t with
      | Close ->
          Log.debug "[rule:close] anchor=%d status=success\n" branch_index;
          Some (Tree.close tree branch_index, formulas)
      | rule_kind -> (
          let output_instance = instantiate_outputs sigma rule.output in
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
              None
          | Some ctx ->
              if not (has_new_formula formulas ctx.branch output_instance) then (
                Log.debug
                  "[rule:apply] status=skip reason=no_new_formula anchor=%d\n"
                  ctx.anchor;
                None)
              else
                push_formulas ctx.tree formulas ctx.branch output_instance
                  ctx.anchor))

let string_of_strategy s =
  let rec aux = function
    | Strategy.AndThen (s1, s2) -> Printf.sprintf "(%s ; %s)" (aux s1) (aux s2)
    | Strategy.OrElse (s1, s2) -> Printf.sprintf "(%s || %s)" (aux s1) (aux s2)
    | Strategy.Fail -> "FAIL"
    | Strategy.Repeat s -> Printf.sprintf "(%s)!" (aux s)
    | Strategy.Rule i -> string_of_int i
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
            match
              apply_rule frame.fvars frame.funcs frame.tree frame.formulas
                (List.nth tableau.rules i)
            with
            | Some (tree, formulas) ->
                prove
                  {
                    tableau with
                    frames =
                      {
                        tree;
                        formulas;
                        fvars = frame.fvars;
                        funcs = frame.funcs;
                        strategy;
                      }
                      :: frames;
                  }
            | None -> prove { tableau with frames })
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
              })
