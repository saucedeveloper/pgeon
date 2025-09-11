type ('key, 'value) t

val create : int -> ('key, 'value) t
(** makes an empty cache of the given capacity (>0)) *)

val size : ('key, 'value) t -> int
(** Current number of entries (<= capacity ) *)

val get : ('key, 'value) t -> 'key -> 'value option
(** get t k moves k to most_recent and returns Some v if present. *)

val put : ('key, 'value) t -> 'key -> 'value -> unit
(** put t k v adds k -> v, evincting the least recent entry if size == capacity
*)
