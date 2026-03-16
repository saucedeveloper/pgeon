open Pgeon

let print_logic (ast : Ast.logic_decl) =
  let print_type_decl (t : Ast.type_decl) = Printf.sprintf "\t%s" t in
  let print_functions_decl (f : Ast.functions_decl) =
    Printf.sprintf "\t%s: %s -> %s" f.name (String.concat " " f.args) f.ret
  in
  let print_binders_decl (b : Ast.binders_decl) =
    Printf.sprintf "\t%s: %s -> %s" b.name b.arg b.ret
  in
  let rec print_expr = function
    | Ast.EVar v -> v
    | Ast.EApp (f, args) ->
        Printf.sprintf "%s(%s)" f
          (String.concat ", " (List.map print_expr args))
    | Ast.EBind (binder, var, body) ->
        Printf.sprintf "%s %s. %s" binder var (print_expr body)
  in
  let print_tree_expr = function
    | branches, rest ->
        String.concat " | "
          (List.map
             (fun (branch, rest) ->
               Printf.sprintf "%s | %s"
                 (String.concat "; " (List.map print_expr branch))
                 rest)
             branches)
        ^ " | " ^ rest
  in
  let print_where_clause (w : Ast.where_clause) =
    match w with
    | Ast.WhereExprClause { dst; src; op } ->
        let op_str =
          match op with
          | Ast.WhereSubstGen { bound; by } ->
              Printf.sprintf "%s <- %s" bound by.name
          | Ast.WhereUnifier { name; left; right } ->
              Printf.sprintf "%s(%s, %s)" name (print_expr left)
                (print_expr right)
        in
        Printf.sprintf "(%s = %s[%s])" dst (print_expr src) op_str
    | Ast.WhereTreeClause { dst; src; op } ->
        let op_str =
          match op with
          | Ast.WhereSubstGen { bound; by } ->
              Printf.sprintf "subst %s by %s" bound by.name
          | Ast.WhereUnifier { name; left; right } ->
              Printf.sprintf "%s(%s, %s)" name (print_expr left)
                (print_expr right)
        in
        Printf.sprintf "(%s = %s[%s])" dst (print_tree_expr src) op_str
  in
  let print_rule_decl (r : Ast.rule_decl) =
    match r with
    | Ast.RuleBranch { name; arrow; lhs; rhs; where } ->
        let arrow_str =
          match arrow with
          | Ast.Close -> "==X"
          | Ast.Invertible -> "==>"
          | Ast.NonInvertible -> "-->"
        in
        let lhs_str = String.concat "; " (List.map print_expr lhs) in
        let rhs_str =
          String.concat " | "
            (List.map (fun r -> String.concat "; " (List.map print_expr r)) rhs)
        in
        Printf.sprintf "\t%s: %s %s %s %s" name lhs_str arrow_str rhs_str
          (String.concat ", " (List.map print_where_clause where))
    | Ast.RuleTree { name; arrow; lhs; rhs; where } ->
        let arrow_str =
          match arrow with
          | Ast.Close -> "->"
          | Ast.Invertible -> "==>"
          | Ast.NonInvertible -> "-->"
        in
        let lhs_str = print_tree_expr lhs in
        let rhs_str = print_tree_expr rhs in
        Printf.sprintf "\t%s: %s %s %s %s" name lhs_str arrow_str rhs_str
          (String.concat ", " (List.map print_where_clause where))
  in
  let rec print_strategy_expr = function
    | Ast.SCall s -> s
    | Ast.SBang s -> Printf.sprintf "%s!" s
    | Ast.SOrElse (e1, e2) ->
        Printf.sprintf "(%s || %s)" (print_strategy_expr e1)
          (print_strategy_expr e2)
    | Ast.SAndThen (e1, e2) ->
        Printf.sprintf "(%s ; %s)" (print_strategy_expr e1)
          (print_strategy_expr e2)
    | Ast.SOrAlt (e1, e2) ->
        Printf.sprintf "(%s &| %s)" (print_strategy_expr e1)
          (print_strategy_expr e2)
    | Ast.SAndAlt (e1, e2) ->
        Printf.sprintf "(%s & %s)" (print_strategy_expr e1)
          (print_strategy_expr e2)
    | Ast.SRepeat e -> Printf.sprintf "(%s)*" (print_strategy_expr e)
  in
  let print_strategy_decl (s : Ast.strategy_decl) =
    Printf.sprintf "\t%s = %s" s.name (print_strategy_expr s.body)
  in
  Log.info
    "\n\
     entry_type: %s;\n\
     types: [\n\
     %s\n\
     ];\n\
     functions: [\n\
     %s\n\
     ];\n\
     binders: [\n\
     %s\n\
     ];\n\
     rules: [\n\
     %s\n\
     ];\n\
     entry_strategy: %s;\n\
     strategies: [\n\
     %s\n\
     ];\n"
    (print_type_decl ast.entry_type)
    (String.concat "\n" (List.map print_type_decl ast.types))
    (String.concat "\n" (List.map print_functions_decl ast.functions))
    (String.concat "\n" (List.map print_binders_decl ast.binders))
    (String.concat "\n " (List.map print_rule_decl ast.rules))
    (print_strategy_decl ast.entry_strategy)
    (String.concat "\n " (List.map print_strategy_decl ast.strategies))

let register_builtins () =
  let rt = Registry.create () in

  Registry.register_generator rt "fresh" (fun st _ ->
      let name = string_of_int st.Tableau.next_fresh in
      let term = Term.Fvar name in
      let st' = { st with Tableau.next_fresh = st.Tableau.next_fresh + 1 } in
      Some (term, st'));

  let get_free_fvars =
    let rec go acc = function
      | Term.Fvar _ as v -> v :: acc
      | Term.App (_, args) -> List.fold_left go acc args
      | Term.Bind (_, body) -> go acc body
      | _ -> acc
    in
    go []
  in

  Registry.register_generator rt "skolem" (fun st src_term ->
      let name = string_of_int st.Tableau.next_symbol in
      let term = Term.App (name, get_free_fvars src_term) in
      let st' = { st with Tableau.next_symbol = st.Tableau.next_symbol + 1 } in
      Some (term, st'));

  Registry.register_unifier rt "mgu" Term.unify;

  rt

let usage_msg =
  "Usage: pgeon [--log-level debug|info|warn|error] <logic-file> <problem-file>"

let parse_level = function
  | "debug" -> Some Log.Debug
  | "info" -> Some Log.Info
  | "warn" -> Some Log.Warn
  | "error" -> Some Log.Error
  | _ -> None

let () =
  let rec parse_args level files = function
    | [] -> Ok (level, List.rev files)
    | "--log-level" :: lvl :: rest -> (
        match (level, parse_level lvl) with
        | Some _, _ -> Error "Multiple --log-level flags provided"
        | None, None ->
            Error
              (Printf.sprintf
                 "Unknown log level '%s'. Expected one of debug, info, warn, \
                  error."
                 lvl)
        | None, Some lvl' -> parse_args (Some lvl') files rest)
    | "--log-level" :: [] ->
        Error "Missing value after --log-level (expected debug|info|warn|error)"
    | arg :: rest -> parse_args level (arg :: files) rest
  in
  let argv = Array.to_list Sys.argv |> List.tl in
  match parse_args None [] argv with
  | Error msg ->
      prerr_endline msg;
      prerr_endline usage_msg;
      exit 1
  | Ok (level_opt, [ logic_file; problem_file ]) -> (
      Log.set_level (Option.value level_opt ~default:Log.Info);
      try
        let l, p =
          ( Parse.parse_logic_file logic_file,
            Parse.parse_problem_file problem_file )
        in
        print_logic l;
        let initial_state, strategy =
          Compile.compile (register_builtins ()) l p
        in
        if Tableau.prove initial_state strategy then (
          print_endline "Success";
          exit 0)
        else (
          print_endline "Failure";
          exit 0)
      with ex ->
        prerr_endline (Printexc.to_string ex);
        print_endline "Error";
        exit 1)
  | Ok (_, _) ->
      prerr_endline usage_msg;
      exit 1
