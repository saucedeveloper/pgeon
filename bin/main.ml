(* bin/main.ml *)
open Pgeon
open Ast
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Parser.MenhirInterpreter

let show text positions =
  E.extract text positions |> E.sanitize |> E.compress |> E.shorten 20

let fail text buffer _ =
  let location = L.range (E.last buffer) in
  let indication =
    Printf.sprintf "Syntax error %s.\n" (E.show (show text) buffer)
  in
  let message = "" in
  Printf.eprintf "%s%s%s%!" location indication message;
  exit 1

let rec string_of_expr = function
  | LVar n -> n
  | LFun (n, el) ->
      Printf.sprintf "%s(%s)" n
        (String.concat ", " (List.map string_of_expr el))
  | LBinder (n, vn, e) -> Printf.sprintf "%s %s. %s" n vn (string_of_expr e)

let rec string_of_strategy = function
  | Rule s -> s
  | AndThen (s1, s2) ->
      "(" ^ string_of_strategy s1 ^ "; " ^ string_of_strategy s2 ^ ")"
  | OrElse (s1, s2) ->
      "(" ^ string_of_strategy s1 ^ " | " ^ string_of_strategy s2 ^ ")"
  | Repeat s -> "(" ^ string_of_strategy s ^ ")*"
  | Try s -> "(" ^ string_of_strategy s ^ ")?"

let log_ast ast =
  List.iter (fun name -> Log.debug "[parse:type] name=%s\n" name) ast.Ast.types;
  List.iter
    (fun (f : function_decl) ->
      Log.debug "[parse:function] name=%s params=[%s] return=%s\n" f.name
        (String.concat "," f.params_types)
        f.t)
    ast.Ast.functions;
  List.iter
    (fun (b : binder_decl) ->
      Log.debug "[parse:binder] name=%s var_type=%s body=%s\n" b.name
        b.variable_type b.t)
    ast.Ast.binders;
  List.iter
    (fun (r : rule_decl) ->
      let arrow =
        match r.arrow with
        | Close -> "==X"
        | NoInvertible -> "-->"
        | Invertible -> "==>"
      in
      let lhs = String.concat "; " (List.map string_of_expr r.lhs) in
      let rhs =
        String.concat " | "
          (List.map
             (fun r -> String.concat "; " (List.map string_of_expr r))
             r.rhs)
      in
      Log.debug "[parse:rule] name=%s arrow=%s lhs=[%s] rhs=[%s]\n" r.name arrow
        lhs rhs)
    ast.Ast.rules;
  List.iter
    (fun ((str, s) : string * strategy_decl) ->
      Log.debug "[parse:strategy] name=%s body=\"%s\"\n" str
        (string_of_strategy s))
    ast.Ast.strategies

let log_problem (problem : Problem.t) =
  let open Problem in
  let { functions; formulas } = problem in
  List.iter
    (fun (f : function_decl) ->
      Log.debug "[problem:function] name=%s params=[%s] return=%s\n" f.name
        (String.concat "," f.params_types)
        f.t)
    functions;
  List.iter
    (fun expr -> Log.debug "[problem:formula] expr=%s\n" (string_of_expr expr))
    formulas

let parse_with checkpoint filename =
  let text, lexbuf = L.read filename in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = checkpoint lexbuf.lex_curr_p in
  I.loop_handle (fun x -> x) (fail text buffer) supplier checkpoint

let merge_unique base additions =
  List.fold_left
    (fun acc name -> if List.mem name acc then acc else acc @ [ name ])
    base additions

let usage_msg = "Usage: pgeon [OPTIONS] LOGIC PROBLEM"
let log_level = ref Log.Info
let logic_file = ref None
let problem_file = ref None

let anon_fun filename =
  match (!logic_file, !problem_file) with
  | None, _ -> logic_file := Some filename
  | _, None -> problem_file := Some filename
  | Some _, Some _ -> raise (Arg.Bad "Too many input files")

let speclist =
  [
    ( "--log-level",
      Arg.String
        (fun level ->
          match level with
          | "debug" -> log_level := Log.Debug
          | "info" -> log_level := Log.Info
          | "warn" -> log_level := Log.Warn
          | "error" -> log_level := Log.Error
          | _ -> raise (Arg.Bad "Invalid log level")),
      "Set log level" );
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  Log.set_level !log_level;
  let logic_file, problem_file =
    match (!logic_file, !problem_file) with
    | Some l, Some p -> (l, p)
    | _ -> failwith "Logic and problem files must be specified"
  in
  let ast = parse_with Parser.Incremental.file logic_file in
  log_ast ast;
  let problem = parse_with Parser.Incremental.problem problem_file in
  log_problem problem;
  if problem.Problem.formulas = [] then (
    Log.error
      "[problem:validate] status=error reason=empty_formula_set file=%s\n"
      problem_file;
    exit 1);
  let funcs =
    merge_unique (Ast.symbol_func ast) (Problem.function_names problem)
  in
  let fvars =
    merge_unique (Ast.symbol_fvar ast) (Problem.symbol_fvar problem)
  in
  let binds = Ast.symbol_bind ast in
  let term_of_expr = Ast.term_of_expr fvars funcs binds in
  let problem_terms = List.map term_of_expr problem.Problem.formulas in
  let tableau = Tableau.init ast ~fvars ~funcs problem_terms in
  Tableau.prove tableau
