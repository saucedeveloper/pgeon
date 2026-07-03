(* list all permutations length n of a list (without duplication of the same
   occurence *)
val perm : int -> 'a list -> 'a list list
val diagonal : 'a Seq.t Seq.t -> 'a Seq.t

val string_repeat : string -> int -> string

(* List.fold_left without the need for a list of elements,
   similar to a pure for loop *)
val aggregate_self : ('a -> 'a) -> 'a -> int -> 'a

val aggregate_self_i : (int -> 'a -> 'a) -> 'a -> int -> 'a

val aggregate_self_until : (int -> 'a -> ('a option)) -> 'a -> int -> 'a

val address_of : 'a -> int

val string_address_of : ?n:int -> 'a -> string

val string_of_bool : bool -> string

(* List.fold_left_map with seq as parameter *)
val seq_fold_left_map_to_list : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a Seq.t -> 'acc * 'b list
