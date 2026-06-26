(* Node of a path string: identifies an index then a symbol *)
type node = {
  index : int;
  symbol : Term_symbol.t;
}

(* Path string: array of index/symbol pairs decribing the traversal of a term *)
type t = node array

val node_root_index : int

val all_of_term : (Term.t -> t list)

val string_of_node : (node -> string)

val string_of : (t -> string)
