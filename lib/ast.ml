type t = {
  types : string list;
  functions : function_decl list;
  binders : binder_decl list;
  rules : rule_decl list;
  strategies : (string * strategy_decl) list;
}

and function_decl = { name : string; params_types : string list; t : string }
and binder_decl = { name : string; variable_type : string; t : string }

and rule_decl = {
  name : string;
  lhs : expr list;
  arrow : rule_type;
  rhs : expr list list;
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
  let aux2 = List.fold_left (aux []) in
  List.fold_left
    (fun acc r -> List.fold_left aux2 (aux2 acc r.lhs) r.rhs)
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
            | Some i -> Term.Fvar i
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
            Log.error
              "[ast:term] status=error reason=unknown_binder name=%s\n" bn;
            exit 1)
  in
  term_of_expr [] e

let main_strategy (ast : t) =
  let rec subst_strategy (env : (string * Strategy.t) list)
      (rules : string list) = function
    | Rule name -> (
        match List.assoc_opt name env with
        | None -> (
            match List.find_index (( = ) name) rules with
            | Some i -> Strategy.Rule i
            | None ->
                Log.error
                  "[strategy:lookup] status=error reason=unknown_rule name=%s\n"
                  name;
                exit 1)
        | Some s -> s)
    | AndThen (a, b) ->
        Strategy.AndThen (subst_strategy env rules a, subst_strategy env rules b)
    | OrElse (a, b) ->
        Strategy.OrElse (subst_strategy env rules a, subst_strategy env rules b)
    | Repeat s -> Strategy.Repeat (subst_strategy env rules s)
    | Try s -> Strategy.OrElse (subst_strategy env rules s, Strategy.Skip)
  in
  let rules = List.map (fun (rd : rule_decl) -> rd.name) ast.rules in
  let env =
    List.fold_left
      (fun env (name, body) ->
        let body = subst_strategy env rules body in
        (name, body) :: env)
      [] ast.strategies
  in
  match List.nth_opt env 0 with
  | None ->
      Log.error
        "[strategy:main] status=error reason=missing_main_strategy\n";
      exit 1
  | Some s -> snd s
