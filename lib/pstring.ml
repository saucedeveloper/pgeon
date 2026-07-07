(* Node of a path string: identifies an index then a symbol *)
type node = {
  index : int;
  symbol : Term_symbol.t;
}

(* Path string: array of index/symbol pairs decribing the traversal of a term *)
type t = node array

let node_root_index = -1

let array_map_to_list (f: 'a -> 'b) (array: 'a array) =
  let array_length = Array.length array in
  let rec recursive index =
    match index with
    | valid when (0 <= valid && valid < array_length) -> (
      let element = Array.get array valid in
      let transformed = f element in
      transformed::(recursive (index + 1))
    )
    | _ -> []
  in
  recursive 0

(*
t = f('x, ~exists.(P(?z)), 'y, P(?z))

t = f(            (* ^.f *)
                  [(-1, f)]
  [0] -> 'x,      (* ^.f.0.'x *) ->
                  [(-1, f); (0, 'x)]
  [1] -> ~exists.( (* ^.f.1.~exists *)
                  [(-1, f); (1, ~exists)]
    [0] -> P(     (* ^.f.1.~exists.0.P *)
                  [(-1, f); (1, ~exists); (0, P)]
      [0] -> ?z   (* ^.f.1.~exists.0.P.0.?z *) ->
                  [(-1, f); (1, ~exists); (0, P); (0, ?z)]
    )
  ),
  [2] -> 'y,      (* ^.f.2.'y *) ->
                  [(-1, f); (2, 'y)]
  [3] -> P(       (* ^.f.3.P *)
                  [(-1, f); (3, P)]
    [0] -> ?z     (* ^.f.3.P.0.?z *) ->
                  [(-1, f); (3, P); (0, ?z)]
  )
)
*)

(* Make all the pstrings / root-to-leaf traversals in `term` *)
let all_of_term (term: Term.t) =
  let rec recursive (term: Term.t) (current_index: int) =
    let created_node = { index = current_index; symbol = Term_symbol.of_term term } in
    (* Dynarray.add_last shared_path created_node; *)
    match term with
    | Term.Bvar _ | Term.Fvar _ | Term.Mvar _ | Term.App (_, []) -> (
      (* let resulting_path = Dynarray.to_array shared_path in *)
      (* Dynarray.remove_last shared_path; *)
      [[created_node]]
    )
    | Term.App (_name, terms) -> (
      let append_recursive index inner =
        let inner_paths = recursive inner index in
        let inner_paths_prepended = List.map (fun inner_path -> created_node::inner_path) inner_paths in
        inner_paths_prepended
      in
      let created_partial_paths = List.mapi append_recursive terms in
      let all_partial_paths = List.concat created_partial_paths in
      (* Dynarray.remove_last shared_path; *)
      all_partial_paths
    )
    | Term.Bind (_name, inner) -> (
      let inner_paths = recursive inner 0 in
      let inner_paths_prepended = List.map (fun inner_path -> created_node::inner_path) inner_paths in
      (* Dynarray.remove_last shared_path; *)
      inner_paths_prepended
    )
  in
  let list_of_lists: node list list = recursive term node_root_index in
  let list_of_arrays = List.map Array.of_list list_of_lists in
  list_of_arrays

(* let get_subterm term index =
  match term with
  | Term.Bvar _ | Term.Fvar _ | Term.Mvar _ -> None
  | Term.App (_name, terms) -> (
    List.nth_opt terms index
  )
  | Term.Bind (_name, term) -> (
    if index = 0 then (Some term) else None
  ) *)

let string_of_node (node: node) =
  let symbol_string = Term_symbol.string_of node.symbol in
  match node.index with
  | -1 (* node_root_index *) -> symbol_string
  | _ -> Printf.sprintf "%d.%s" node.index symbol_string

let string_of (pstr: t) = String.concat "." (array_map_to_list string_of_node pstr)
