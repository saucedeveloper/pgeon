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

type t = {
  frames : frame list;
  rules : rule_def list;
  strategy_env : (string * Strategy.t) list;
}

val init : Ast.t -> fvars:string list -> funcs:string list -> Term.t list -> t
val join_map : string -> ('a -> string) -> 'a list -> string
val perm_n : 'a list -> int -> 'a list list

type match_result = {
  sigma : (Term.name * Term.t) list;
  inputs : int list;
}

type branch_context = {
  tree : Tree.t;
  branch : int list;
  anchor : int;
}

val branch_index : 'a list -> 'a option
val push_formulas :
  Tree.t ->
  Term.t list ->
  int list ->
  Term.t list list ->
  int ->
  (Tree.t * Term.t list) option
val instantiate_outputs :
  (Term.name * Term.t) list -> Term.t list list -> Term.t list list
val find_match :
  Term.t list -> int list -> rule_def -> match_result option
val term_in_branch : Term.t list -> int list -> Term.t -> bool
val has_new_formula :
  Term.t list -> int list -> Term.t list list -> bool
val index_in_branch : 'a list -> 'a -> int option
val removal_order : 'a list -> 'a list -> 'a list
val merge_children : Tree.t -> Tree.t list -> Tree.t list option
val remove_root : Tree.t -> int -> Tree.t option
val remove_under_parent : Tree.t -> int -> int -> Tree.t option
val remove_inputs :
  Tree.t -> int list -> int -> int list -> branch_context option
val apply_rule :
  Tree.t -> Term.t list -> rule_def -> (Tree.t * Term.t list) option
val string_of_strategy : Strategy.t list -> string
val branch : int * int -> Tree.t -> int * int
val prove : t -> unit
