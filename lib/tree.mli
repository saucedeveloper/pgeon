type t = {
  node : int;
  childs : t list option;
}

val init : 'a list -> t
val close : t -> int -> t
val find_leftmost_branch : t -> (int list * int) option
