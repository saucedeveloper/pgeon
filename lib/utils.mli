(* list all permutations length n of a list (without duplication of the same
   occurence *)
val perm : int -> 'a list -> 'a list list
val diagonal : 'a Seq.t Seq.t -> 'a Seq.t
