(* list all permutations length n of a list (without duplication of the same
   occurence *)
val perm : int -> 'a list -> 'a list list
val diagonal : 'a Seq.t Seq.t -> 'a Seq.t

val string_repeat : string -> int -> string

(* List.fold_left without the need for a list of elements,
   similar to a pure for loop *)
val aggregate_self : ('a -> 'a) -> 'a -> count: int -> 'a
