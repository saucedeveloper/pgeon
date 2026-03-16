open Ast

exception Parse_error of string

let parse_errorf fmt = Printf.ksprintf (fun s -> raise (Parse_error s)) fmt

type token_kind =
  | IDENT of string
  | KW_MAIN
  | KW_TYPE
  | KW_FUNCTION
  | KW_BINDER
  | KW_RULE
  | KW_TREE
  | KW_STRATEGY
  | KW_WHERE
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | COLON
  | COMMA
  | SEMI
  | BAR
  | DOT
  | EQ
  | AT
  | ELLIPSIS
  | ARROW_CLOSE (* ==X *)
  | ARROW_INV (* ==> *)
  | ARROW_NONINV (* --> *)
  | ARROW_REWRITE (* <-  *)
  | ARROW_TYPE (* ->  *)
  | OROR (* ||  *)
  | STAR (* *   *)
  | BANG (* !   *)
  | QMARK (* ?   *)
  | AMPEROR (* &|  *)
  | AMPERSAND (* &   *)
  | EOF

type token = { kind : token_kind; line : int; col : int }

let string_of_token_kind = function
  | IDENT s -> Printf.sprintf "identifier %S" s
  | KW_MAIN -> "main"
  | KW_TYPE -> "type"
  | KW_FUNCTION -> "function"
  | KW_BINDER -> "binder"
  | KW_RULE -> "rule"
  | KW_TREE -> "tree"
  | KW_STRATEGY -> "strategy"
  | KW_WHERE -> "where"
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | LBRACK -> "["
  | RBRACK -> "]"
  | COLON -> ":"
  | COMMA -> ","
  | SEMI -> ";"
  | BAR -> "|"
  | DOT -> "."
  | EQ -> "="
  | AT -> "@"
  | ELLIPSIS -> "..."
  | ARROW_CLOSE -> "==X"
  | ARROW_INV -> "==>"
  | ARROW_NONINV -> "-->"
  | ARROW_REWRITE -> "<-"
  | ARROW_TYPE -> "->"
  | OROR -> "||"
  | STAR -> "*"
  | BANG -> "!"
  | QMARK -> "?"
  | AMPEROR -> "&|"
  | AMPERSAND -> "&"
  | EOF -> "end of file"

let token_starts_expr = function IDENT _ | LPAREN -> true | _ -> false

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_continue = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let keyword_or_ident s =
  match s with
  | "main" -> KW_MAIN
  | "type" -> KW_TYPE
  | "function" -> KW_FUNCTION
  | "binder" -> KW_BINDER
  | "rule" -> KW_RULE
  | "tree" -> KW_TREE
  | "strategy" -> KW_STRATEGY
  | "where" -> KW_WHERE
  | _ -> IDENT s

let lex_string (input : string) : token list =
  let len = String.length input in
  let rec skip_comment i line col =
    if i >= len then
      parse_errorf "unterminated comment at line %d, column %d" line col
    else if i + 1 < len && input.[i] = '*' && input.[i + 1] = '/' then
      (i + 2, line, col + 2)
    else if input.[i] = '\n' then skip_comment (i + 1) (line + 1) 1
    else skip_comment (i + 1) line (col + 1)
  in
  let rec scan_ident j =
    if j < len && is_ident_continue input.[j] then scan_ident (j + 1) else j
  in
  let rec loop i line col acc =
    if i >= len then List.rev ({ kind = EOF; line; col } :: acc)
    else
      match input.[i] with
      | ' ' | '\t' | '\r' -> loop (i + 1) line (col + 1) acc
      | '\n' -> loop (i + 1) (line + 1) 1 acc
      | '/' when i + 1 < len && input.[i + 1] = '*' ->
          let i, line, col = skip_comment (i + 2) line (col + 2) in
          loop i line col acc
      | '(' -> loop (i + 1) line (col + 1) ({ kind = LPAREN; line; col } :: acc)
      | ')' -> loop (i + 1) line (col + 1) ({ kind = RPAREN; line; col } :: acc)
      | '{' -> loop (i + 1) line (col + 1) ({ kind = LBRACE; line; col } :: acc)
      | '}' -> loop (i + 1) line (col + 1) ({ kind = RBRACE; line; col } :: acc)
      | '[' -> loop (i + 1) line (col + 1) ({ kind = LBRACK; line; col } :: acc)
      | ']' -> loop (i + 1) line (col + 1) ({ kind = RBRACK; line; col } :: acc)
      | ':' -> loop (i + 1) line (col + 1) ({ kind = COLON; line; col } :: acc)
      | ',' -> loop (i + 1) line (col + 1) ({ kind = COMMA; line; col } :: acc)
      | ';' -> loop (i + 1) line (col + 1) ({ kind = SEMI; line; col } :: acc)
      | '|' when i + 1 < len && input.[i + 1] = '|' ->
          loop (i + 2) line (col + 2) ({ kind = OROR; line; col } :: acc)
      | '|' -> loop (i + 1) line (col + 1) ({ kind = BAR; line; col } :: acc)
      | '.' when i + 2 < len && input.[i + 1] = '.' && input.[i + 2] = '.' ->
          loop (i + 3) line (col + 3) ({ kind = ELLIPSIS; line; col } :: acc)
      | '.' -> loop (i + 1) line (col + 1) ({ kind = DOT; line; col } :: acc)
      | '=' when i + 2 < len && input.[i + 1] = '=' && input.[i + 2] = 'X' ->
          loop (i + 3) line (col + 3) ({ kind = ARROW_CLOSE; line; col } :: acc)
      | '=' when i + 2 < len && input.[i + 1] = '=' && input.[i + 2] = '>' ->
          loop (i + 3) line (col + 3) ({ kind = ARROW_INV; line; col } :: acc)
      | '=' -> loop (i + 1) line (col + 1) ({ kind = EQ; line; col } :: acc)
      | '-' when i + 2 < len && input.[i + 1] = '-' && input.[i + 2] = '>' ->
          loop (i + 3) line (col + 3) ({ kind = ARROW_NONINV; line; col } :: acc)
      | '-' when i + 1 < len && input.[i + 1] = '>' ->
          loop (i + 2) line (col + 2) ({ kind = ARROW_TYPE; line; col } :: acc)
      | '<' when i + 1 < len && input.[i + 1] = '-' ->
          loop (i + 2) line (col + 2)
            ({ kind = ARROW_REWRITE; line; col } :: acc)
      | '@' -> loop (i + 1) line (col + 1) ({ kind = AT; line; col } :: acc)
      | '*' -> loop (i + 1) line (col + 1) ({ kind = STAR; line; col } :: acc)
      | '!' -> loop (i + 1) line (col + 1) ({ kind = BANG; line; col } :: acc)
      | '?' -> loop (i + 1) line (col + 1) ({ kind = QMARK; line; col } :: acc)
      | '&' when i + 1 < len && input.[i + 1] = '|' ->
          loop (i + 2) line (col + 2) ({ kind = AMPEROR; line; col } :: acc)
      | '&' ->
          loop (i + 1) line (col + 1) ({ kind = AMPERSAND; line; col } :: acc)
      | c when is_ident_start c ->
          let j = scan_ident (i + 1) in
          let s = String.sub input i (j - i) in
          let kind = keyword_or_ident s in
          loop j line (col + (j - i)) ({ kind; line; col } :: acc)
      | c ->
          parse_errorf "unexpected character %C at line %d, column %d" c line
            col
  in
  loop 0 1 1 []

type parser = { tokens : token array; mutable pos : int }

let parser_of_tokens toks = { tokens = Array.of_list toks; pos = 0 }

let current p =
  if p.pos < Array.length p.tokens then p.tokens.(p.pos)
  else p.tokens.(Array.length p.tokens - 1)

let current_kind p = (current p).kind

let advance p =
  let tok = current p in
  if p.pos < Array.length p.tokens then p.pos <- p.pos + 1;
  tok

let error_at tok fmt =
  Printf.ksprintf
    (fun s ->
      raise
        (Parse_error
           (Printf.sprintf "%s at line %d, column %d" s tok.line tok.col)))
    fmt

let expect_kind p expected =
  let tok = advance p in
  if tok.kind <> expected then
    error_at tok "expected %s, got %s"
      (string_of_token_kind expected)
      (string_of_token_kind tok.kind)

let consume_if p kind =
  if current_kind p = kind then (
    ignore (advance p);
    true)
  else false

let expect_ident p =
  match advance p with
  | { kind = IDENT s; _ } -> s
  | tok ->
      error_at tok "expected identifier, got %s" (string_of_token_kind tok.kind)

let peek_n_kind p n =
  let idx = p.pos + n in
  if idx < Array.length p.tokens then p.tokens.(idx).kind else EOF

let parse_arrow_kind p =
  match advance p with
  | { kind = ARROW_CLOSE; _ } -> Close
  | { kind = ARROW_INV; _ } -> Invertible
  | { kind = ARROW_NONINV; _ } -> NonInvertible
  | tok ->
      error_at tok "expected rule arrow, got %s" (string_of_token_kind tok.kind)

let ensure_no_bang_or_qmark p context =
  match current_kind p with
  | BANG | QMARK ->
      error_at (current p) "%s using !, ?, or !? is not supported yet" context
  | _ -> ()

let rec parse_expr p =
  match current_kind p with
  | IDENT name -> begin
      match (peek_n_kind p 1, peek_n_kind p 2) with
      | IDENT _, DOT ->
          ignore (advance p);
          let bound = expect_ident p in
          expect_kind p DOT;
          let body = parse_expr p in
          EBind (name, bound, body)
      | LPAREN, _ ->
          ignore (advance p);
          expect_kind p LPAREN;
          let args = parse_comma_separated_until p RPAREN parse_expr in
          EApp (name, args)
      | _ ->
          ignore (advance p);
          EVar name
    end
  | LPAREN ->
      ignore (advance p);
      let e = parse_expr p in
      expect_kind p RPAREN;
      e
  | tok ->
      error_at (current p) "expected expression, got %s"
        (string_of_token_kind tok)

and parse_comma_separated_until p closing parse_one =
  if current_kind p = closing then (
    ignore (advance p);
    [])
  else
    let rec loop acc =
      let x = parse_one p in
      let acc = x :: acc in
      match current_kind p with
      | COMMA ->
          ignore (advance p);
          loop acc
      | k when k = closing ->
          ignore (advance p);
          List.rev acc
      | tok ->
          error_at (current p) "expected , or %s, got %s"
            (string_of_token_kind closing)
            (string_of_token_kind tok)
    in
    loop []

let parse_expr_list_until_branch_end p =
  let first = parse_expr p in
  let rec loop acc =
    match current_kind p with
    | SEMI when peek_n_kind p 1 = ELLIPSIS -> List.rev acc
    | SEMI when token_starts_expr (peek_n_kind p 1) ->
        ignore (advance p);
        let e = parse_expr p in
        loop (e :: acc)
    | _ -> List.rev acc
  in
  loop [ first ]

let parse_optional_branch_rest p =
  if consume_if p SEMI then (
    expect_kind p ELLIPSIS;
    expect_ident p)
  else ""

let parse_branch_expr p =
  expect_kind p LPAREN;
  let exprs, rest =
    match current_kind p with
    | ELLIPSIS ->
        ignore (advance p);
        let r = expect_ident p in
        ([], r)
    | RPAREN -> ([], "")
    | _ ->
        let exprs = parse_expr_list_until_branch_end p in
        let rest =
          if current_kind p = SEMI then (
            ignore (advance p);
            expect_kind p ELLIPSIS;
            expect_ident p)
          else ""
        in
        (exprs, rest)
  in
  expect_kind p RPAREN;
  (exprs, rest)

let rec parse_tree_expr p =
  match current_kind p with
  | ELLIPSIS ->
      ignore (advance p);
      let rest = expect_ident p in
      ([], rest)
  | LPAREN -> begin
      match peek_n_kind p 1 with
      | LPAREN | ELLIPSIS ->
          (* grouped tree expr: ((...) | ...T) or (...B | ...T) *)
          expect_kind p LPAREN;
          let t = parse_tree_expr p in
          expect_kind p RPAREN;
          t
      | _ ->
          (* ordinary tree expr starting with a branch: (...) | ...T *)
          parse_tree_expr_body p
    end
  | tok ->
      error_at (current p) "expected tree expression, got %s"
        (string_of_token_kind tok)

and parse_tree_expr_body p =
  let first_branch = parse_branch_expr p in
  let rec loop branches rest =
    if consume_if p BAR then
      match current_kind p with
      | ELLIPSIS ->
          ignore (advance p);
          let rest = expect_ident p in
          loop branches rest
      | LPAREN ->
          let br = parse_branch_expr p in
          loop (br :: branches) rest
      | tok ->
          error_at (current p)
            "expected branch expression or tree rest after |, got %s"
            (string_of_token_kind tok)
    else (List.rev branches, rest)
  in
  loop [ first_branch ] ""

let parse_expr_list_until_branch_sep p =
  let first = parse_expr p in
  let rec loop acc =
    match current_kind p with
    | SEMI when token_starts_expr (peek_n_kind p 1) ->
        ignore (advance p);
        let e = parse_expr p in
        loop (e :: acc)
    | _ -> List.rev acc
  in
  loop [ first ]

let parse_rule_branch_rhs p =
  let first = parse_expr_list_until_branch_sep p in
  let rec loop branches =
    if consume_if p BAR then
      let next_branch = parse_expr_list_until_branch_sep p in
      loop (next_branch :: branches)
    else List.rev branches
  in
  loop [ first ]

let parse_function_decl_after_keyword p =
  let rec collect_names acc =
    match current_kind p with
    | IDENT _ ->
        let name = expect_ident p in
        collect_names (name :: acc)
    | COLON -> List.rev acc
    | tok ->
        error_at (current p) "expected function name or :, got %s"
          (string_of_token_kind tok)
  in
  let names = collect_names [] in
  expect_kind p COLON;
  let rec collect_arg_types acc =
    match current_kind p with
    | IDENT _ ->
        let t = expect_ident p in
        collect_arg_types (t :: acc)
    | ARROW_TYPE -> List.rev acc
    | tok ->
        error_at (current p) "expected type name or ->, got %s"
          (string_of_token_kind tok)
  in
  let args = collect_arg_types [] in
  expect_kind p ARROW_TYPE;
  let ret = expect_ident p in
  List.map (fun name -> { name; args; ret }) names

let parse_binder_decl_after_keyword p =
  let name = expect_ident p in
  expect_kind p COLON;
  let arg = expect_ident p in
  expect_kind p DOT;
  let ret = expect_ident p in
  { name; arg; ret }

let parse_where_decl p =
  let parse_gen_call p =
    expect_kind p AT;
    let name = expect_ident p in
    expect_kind p LPAREN;
    expect_kind p RPAREN;
    { name }
  in

  let parse_where_op p =
    match current_kind p with
    | IDENT _ ->
        let first = expect_ident p in
        begin match current_kind p with
        | ARROW_REWRITE ->
            expect_kind p ARROW_REWRITE;
            let by = parse_gen_call p in
            WhereSubstGen { bound = first; by }
        | LPAREN ->
            expect_kind p LPAREN;
            let left = parse_expr p in
            expect_kind p COMMA;
            let right = parse_expr p in
            expect_kind p RPAREN;
            WhereUnifier { name = first; left; right }
        | _ ->
            error_at (current p)
              "expected '<-' for substitution generator or '(' for where \
               operator call"
        end
    | _ -> error_at (current p) "expected where operator"
  in

  let parse_where_clause p =
    match current_kind p with
    | IDENT _ ->
        let dst = expect_ident p in
        expect_kind p EQ;
        let src = parse_expr p in
        expect_kind p LBRACK;
        let op = parse_where_op p in
        expect_kind p RBRACK;
        WhereExprClause { dst; src; op }
    | ELLIPSIS ->
        expect_kind p ELLIPSIS;
        let dst = expect_ident p in
        expect_kind p EQ;
        let src = parse_tree_expr p in
        expect_kind p LBRACK;
        let op = parse_where_op p in
        expect_kind p RBRACK;
        WhereTreeClause { dst; src; op }
    | _ ->
        error_at (current p)
          "expected metavariable or tree variable in where clause"
  in

  expect_kind p KW_WHERE;
  expect_kind p LBRACE;
  let rec loop acc =
    match current_kind p with
    | RBRACE ->
        ignore (advance p);
        List.rev acc
    | EOF -> error_at (current p) "unterminated where clause"
    | _ ->
        let clause = parse_where_clause p in
        loop (clause :: acc)
  in
  loop []

let parse_rule_decl_after_keyword p =
  let name = expect_ident p in
  expect_kind p COLON;
  let lhs = parse_expr_list_until_branch_sep p in
  let arrow = parse_arrow_kind p in
  let rhs =
    if token_starts_expr (current_kind p) then parse_rule_branch_rhs p else []
  in
  let where = if current_kind p = KW_WHERE then parse_where_decl p else [] in
  RuleBranch { name; arrow; lhs; rhs; where }

let parse_tree_rule_decl_after_keywords p =
  let name = expect_ident p in
  expect_kind p COLON;
  let lhs = parse_tree_expr p in
  let arrow = parse_arrow_kind p in
  let rhs = parse_tree_expr p in
  let where = if current_kind p = KW_WHERE then parse_where_decl p else [] in
  RuleTree { name; arrow; lhs; rhs; where }

let rec parse_strategy_expr p = parse_strategy_choice p

and parse_strategy_choice p =
  let left = parse_strategy_comp p in
  parse_strategy_choice_tail p left

and parse_strategy_choice_tail p left =
  if consume_if p OROR then
    let right = parse_strategy_comp p in
    parse_strategy_choice_tail p (Ast.SOrElse (left, right))
  else if consume_if p AMPEROR then
    let right = parse_strategy_comp p in
    parse_strategy_choice_tail p (Ast.SOrAlt (left, right))
  else left

and parse_strategy_comp p =
  let left = parse_strategy_postfix p in
  parse_strategy_comp_tail p left

and parse_strategy_comp_tail p left =
  if consume_if p SEMI then
    let right = parse_strategy_postfix p in
    parse_strategy_comp_tail p (Ast.SAndThen (left, right))
  else if consume_if p AMPERSAND then
    let right = parse_strategy_postfix p in
    parse_strategy_comp_tail p (Ast.SAndAlt (left, right))
  else left

and parse_strategy_postfix p =
  let base = parse_strategy_atom p in
  (* ensure_no_bang_or_qmark p "strategy postfix"; *)
  if consume_if p BANG then
    SBang
      (match base with
      | SCall name -> name
      | _ -> error_at (current p) "only rule calls can be used with !")
  else if consume_if p STAR then SRepeat base
  else base

and parse_strategy_atom p =
  match current_kind p with
  | IDENT name ->
      ignore (advance p);
      SCall name
  | LPAREN ->
      ignore (advance p);
      let s = parse_strategy_expr p in
      expect_kind p RPAREN;
      s
  | tok ->
      error_at (current p) "expected strategy expression, got %s"
        (string_of_token_kind tok)

let parse_strategy_decl_after_keyword p =
  let name = expect_ident p in
  expect_kind p COLON;
  let body = parse_strategy_expr p in
  { name; body }

let parse_logic_string s =
  let p = parser_of_tokens (lex_string s) in
  let entry_type = ref None in
  let types = ref [] in
  let functions = ref [] in
  let binders = ref [] in
  let rules = ref [] in
  let strategies = ref [] in
  let entry_strategy = ref None in
  let rec loop () =
    match current_kind p with
    | EOF -> ()
    | KW_MAIN ->
        ignore (advance p);
        begin match current_kind p with
        | KW_TYPE ->
            ignore (advance p);
            let t = expect_ident p in
            entry_type := Some t;
            loop ()
        | KW_STRATEGY ->
            ignore (advance p);
            (* ignore (expect_ident p); *)
            (* expect_kind p COLON; *)
            let sdecl = parse_strategy_decl_after_keyword p in
            entry_strategy := Some sdecl;
            loop ()
        | tok ->
            error_at (current p) "expected type or strategy after main, got %s"
              (string_of_token_kind tok)
        end
    | KW_TYPE ->
        ignore (advance p);
        let t = expect_ident p in
        types := t :: !types;
        loop ()
    | KW_FUNCTION ->
        ignore (advance p);
        let ds = parse_function_decl_after_keyword p in
        functions := List.rev_append ds !functions;
        loop ()
    | KW_BINDER ->
        ignore (advance p);
        let b = parse_binder_decl_after_keyword p in
        binders := b :: !binders;
        loop ()
    | KW_RULE ->
        ignore (advance p);
        let r = parse_rule_decl_after_keyword p in
        rules := r :: !rules;
        loop ()
    | KW_TREE ->
        ignore (advance p);
        expect_kind p KW_RULE;
        let r = parse_tree_rule_decl_after_keywords p in
        rules := r :: !rules;
        loop ()
    | KW_STRATEGY ->
        ignore (advance p);
        let sdecl = parse_strategy_decl_after_keyword p in
        strategies := sdecl :: !strategies;
        loop ()
    | tok ->
        error_at (current p) "unexpected token at top level of logic file: %s"
          (string_of_token_kind tok)
  in
  loop ();
  let entry_type =
    match !entry_type with
    | Some t -> t
    | None -> parse_errorf "logic file is missing a main type declaration"
  in
  let entry_strategy =
    match !entry_strategy with
    | Some s -> s
    | None -> parse_errorf "logic file is missing a main strategy declaration"
  in
  {
    entry_type;
    types = List.rev !types;
    functions = List.rev !functions;
    binders = List.rev !binders;
    rules = List.rev !rules;
    entry_strategy;
    strategies = entry_strategy :: List.rev !strategies;
  }

let parse_problem_string s =
  let p = parser_of_tokens (lex_string s) in
  let functions = ref [] in
  let formulas = ref [] in
  let rec loop () =
    match current_kind p with
    | EOF -> ()
    | KW_FUNCTION ->
        ignore (advance p);
        let ds = parse_function_decl_after_keyword p in
        functions := List.rev_append ds !functions;
        loop ()
    | tok when token_starts_expr tok ->
        let e = parse_expr p in
        formulas := e :: !formulas;
        ignore (consume_if p SEMI);
        loop ()
    | tok ->
        error_at (current p) "unexpected token at top level of problem file: %s"
          (string_of_token_kind tok)
  in
  loop ();
  { functions = List.rev !functions; formulas = List.rev !formulas }

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let parse_logic_file path = parse_logic_string (read_file path)
let parse_problem_file path = parse_problem_string (read_file path)
