type t

val init : Term.t list -> t
val has_open_branches : t -> bool
val get_open_branches : t -> (int * Term.t list) Seq.t

(* close all the branches that match the predicate, gives false if no branches matched *)
val close : t -> (Term.t list -> bool) -> t * bool

(* remove the branch with the given id *)
val remove : t -> int -> t
val add_branch : t -> Term.t list -> t
val map_formulas : (Term.t -> Term.t) -> t -> t
