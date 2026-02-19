type t = {
  types : string list;
  functions : function_decl list;
  binders : binder_decl list;
  rules : rule_decl list;
  strategies : (string * strategy_decl) list;
}

and function_decl = { name : string; params_types : string list; t : string }
and binder_decl = { name : string; variable_type : string; t : string }

and tree_expr =
  | TreeLeaf of expr
  | TreeBranch of expr list
  | TreeUnion of tree_expr * tree_expr

and tree_rule = {
  lhs_tree : tree_expr;
  rhs_tree : tree_expr;
  branch_tail : string option;
  tree_var : string;
}

and generator_call = {
  gen_name : string;
  gen_args : expr list;
}

and subst_rhs =
  | SR_Gen of generator_call
  | SR_Expr of where_expr

and subst_entry = {
  target : string;
  rhs : subst_rhs;
}

and where_base =
  | WExpr of expr
  | WTree of tree_expr

and where_subst =
  | SubstEntries of subst_entry list
  | SubstRef of expr

and where_expr = {
  base : where_base;
  substs : where_subst option;
}

and where_binding = {
  var : string;
  value : where_expr;
}

and rule_decl = {
  name : string;
  lhs : expr list;
  arrow : rule_type;
  rhs : expr list list;
  where_clause : where_binding list;
  meta_envs : (string * string list) list;
  tree_rule : tree_rule option;
}

and rule_type = Close | NoInvertible | Invertible

and expr =
  | LVar of string
  | LFun of string * expr list
  | LBinder of string * string * expr

and strategy_decl =
  | Rule of string
  | AndThen of strategy_decl * strategy_decl
  | OrElse of strategy_decl * strategy_decl
  | Repeat of strategy_decl
  | Try of strategy_decl

let is_meta name =
  String.length name > 0
  && let c = name.[0] in
     Char.uppercase_ascii c = c && Char.lowercase_ascii c <> c

let symbol_fvar ast =
  let rec aux env acc = function
    | LVar n -> (
        match List.mem n env with
        | true -> acc
        | false -> (
            match List.mem n acc with true -> acc | false -> n :: acc))
    | LFun (_, el) -> List.fold_left (aux env) acc el
    | LBinder (_, n, e) -> aux (n :: env) acc e
  in
  let rec aux_tree env acc = function
    | TreeLeaf e -> aux env acc e
    | TreeBranch el -> List.fold_left (aux env) acc el
    | TreeUnion (a, b) ->
        let acc = aux_tree env acc a in
        aux_tree env acc b
  in
  let aux_where_base env acc = function
    | WExpr e -> aux env acc e
    | WTree tree -> aux_tree env acc tree
  in
  let rec aux_where_expr env acc (w : where_expr) =
    let acc = aux_where_base env acc w.base in
    match w.substs with
    | None -> acc
    | Some (SubstEntries entries) ->
        List.fold_left (aux_subst env) acc entries
    | Some (SubstRef expr) -> aux env acc expr
  and aux_subst env acc (s : subst_entry) =
    match s.rhs with
    | SR_Gen call ->
        List.fold_left (aux env) acc call.gen_args
    | SR_Expr wexpr -> aux_where_expr env acc wexpr
  in
  let aux2 = List.fold_left (aux []) in
  List.fold_left
    (fun acc r ->
      let acc = List.fold_left aux2 (aux2 acc r.lhs) r.rhs in
      let acc =
        match r.tree_rule with
        | None -> acc
        | Some tr ->
            let acc = aux_tree [] acc tr.lhs_tree in
            aux_tree [] acc tr.rhs_tree
      in
      List.fold_left
        (fun acc binding -> aux_where_expr [] acc binding.value)
        acc r.where_clause)
    [] ast.rules

let symbol_func ast = List.map (fun (f : function_decl) -> f.name) ast.functions
let symbol_bind ast = List.map (fun (b : binder_decl) -> b.name) ast.binders

let term_of_expr fvars funcs binds e =
  let rec term_of_expr b_env = function
    | LVar n -> (
        match List.find_index (( = ) n) b_env with
        | Some i -> Term.Bvar i
        | None -> (
            match List.find_index (( = ) n) fvars with
            | Some i -> Term.Mvar i
            | None ->
                Log.error
                  "[ast:term] status=error reason=unknown_free_variable name=%s\n"
                  n;
                exit 1))
    | LFun (fn, el) -> (
        match List.find_index (( = ) fn) funcs with
        | Some fn -> Term.App (fn, List.map (term_of_expr b_env) el)
        | None ->
            Log.error
              "[ast:term] status=error reason=unknown_function name=%s\n" fn;
            exit 1)
    | LBinder (bn, vn, e) -> (
        match List.find_index (( = ) bn) binds with
        | Some bn -> Term.Bind (bn, term_of_expr (vn :: b_env) e)
        | None ->
            Log.error "[ast:term] status=error reason=unknown_binder name=%s\n"
              bn;
            exit 1)
  in
  term_of_expr [] e

let compile_strategies (ast : t) =
  let rules = List.map (fun (rd : rule_decl) -> rd.name) ast.rules in
  let strategy_names = List.map fst ast.strategies in
  let rec compile = function
    | Rule name -> (
        if List.exists (( = ) name) strategy_names then Strategy.Call name
        else
          match List.find_index (( = ) name) rules with
          | Some i -> Strategy.Rule i
          | None ->
              Log.error
                "[strategy:lookup] status=error reason=unknown_name name=%s\n"
                name;
              exit 1)
    | AndThen (a, b) -> Strategy.AndThen (compile a, compile b)
    | OrElse (a, b) -> Strategy.OrElse (compile a, compile b)
    | Repeat s -> Strategy.Repeat (compile s)
    | Try s -> Strategy.OrElse (compile s, Strategy.Skip)
  in
  let compiled =
    List.map (fun (name, body) -> (name, compile body)) ast.strategies
  in
  match List.rev compiled with
  | [] ->
      Log.error "[strategy:main] status=error reason=missing_main_strategy\n";
      exit 1
  | (_, main) :: _ -> (compiled, main)

(* -------------------------------------------------------------------------- *)
(* Placeholder API for future tableau compilation steps.                      *)
(* The concrete implementation was intentionally removed during refactoring.  *)

[@@@ocaml.warning "-32-33-34-69"]

type compiled_rule = unit
type compiled_tableau = { rules : compiled_rule list }

let compile_rules (_ast : t) ~fvars:_ ~funcs:_ =
  failwith "Ast.compile_rules is not implemented yet"

let compile_tableau (_ast : t) ~fvars ~funcs =
  ignore (fvars, funcs);
  failwith "Ast.compile_tableau is not implemented yet"

[@@@ocaml.warning "+32+33+34+69"]
