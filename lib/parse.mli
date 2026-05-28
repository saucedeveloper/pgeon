exception Parse_error of string

val parse_logic_string : string -> Ast.logic_decl
(** Parse a logic specification. *)

val parse_problem_string : string -> Ast.problem_decl
(** Parse a problem instance. *)

val parse_logic_file : string -> Ast.logic_decl
val parse_problem_file : string -> Ast.problem_decl
