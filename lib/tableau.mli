type formula = int * Term.t (* id, term *)

type proof_tree =
  formula list list (* list of branches, each branch is a list of terms *)

val is_closed : proof_tree -> bool

type application_key = int * int list (* rule id, sorted list of formula ids *)

type proof_state = {
  tree : proof_tree;
  next_fresh : int;
  next_symbol : int;
  next_formula_id : int;
  applied : application_key list; (* cache of applied rules per formula *)
}

type strategy = proof_state -> proof_state Seq.t

type rule = {
  id : int;
  run : proof_state -> proof_state Seq.t;
  (* Deterministic eager application. [None] means the rule cannot be used with
     the strategy postfix [!]. *)
  run_bang : (proof_state -> proof_state option) option;
}

val skip : strategy
val fail : strategy

(* Left-biased choice: try the first strategy, then the second only if the
   first produces no result. *)
val orElse : strategy -> strategy -> strategy

val andThen : strategy -> strategy -> strategy

(* Fair interleaving of alternatives. *)
val orAlt : strategy -> strategy -> strategy

(* Fair sequencing using diagonalization. *)
val andAlt : strategy -> strategy -> strategy

(* Repetition, including the zero-step result. *)
val repeat : strategy -> strategy

val applyRule : rule -> strategy
val applyRuleBang : rule -> strategy

(* main engine *)
val prove : proof_state -> strategy -> bool
