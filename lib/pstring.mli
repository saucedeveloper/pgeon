(* Path string in an index as defined in
the Handbook of Automated Reasoning *)

(* Node of a path string: identifies an index then a symbol *)
type node = {
  index : int;
  symbol : Term_symbol.t;
}

(* Path string: array of index/symbol pairs decribing the traversal of a term.
It is assumed to be used immutably *)
type t = node array

(* The index value for the root of an index (specific value with no meaning) *)
val node_root_index : int

(* All path strings of this term *)
val all_of_term : (Term.t -> t list)

(* Path string node to string *)
val string_of_node : (node -> string)

(* Path string to string *)
val string_of : (t -> string)
