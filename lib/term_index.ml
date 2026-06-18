(*
Path index for
t = f('x, exists.(P(?z)), 'y, P(?z))

               (0)
                | f
               (1)
     ///////////-\\\\\\\
  0 /       1 /   \ 2   \ 3
  (2)       (3)   (4)   (5)
'x |  exists |  'y |   P |
  (6)       (7)   (8)   (11)
  {t}      0 |    {t}  0 |
            (10)        (13)
           P |        ?z |
            (12)        (15)
           0 |          {t}
            (14)
          ?z |
            (16)
            {t}
*)

(* Module for Set implementation *)
module IdComparableTerm = struct
  type t = Factory.term
  let compare a b = compare (Factory.address_of a) (Factory.address_of b)
end

(* Set of terms on a leaf of the index *)
module TermSet = Set.Make(IdComparableTerm)

type term_set = TermSet.t

let string_of_term_set (term_set: term_set) =
  let term_list = TermSet.to_list term_set in
  let string_of_term term = Factory.string_address_of term in
  let strings = List.map string_of_term term_list in
  Printf.sprintf "{ %s }" (String.concat ", " strings)

(* Module for Array node implementation *)
module MapIndexKey = struct
  type t = int
  let compare a b = compare a b
end

(* Array with not necessarilly contiguous index keys *)
module SparseArray = Map.Make(MapIndexKey)

(* Module for Map node implementation *)
module MapTermSymbolKey = struct
  type t = Term_symbol.t
  let compare a b = compare a b
end

(* Dictionary of term symbol to map subnode *)
module SymbolKeyedMap = Map.Make(MapTermSymbolKey)

(* Node that contains subnodes based on argument position *)
type index_array_node = index_map_node SparseArray.t

(* Subnode of map node: either a sub array or a leaf containing the set of matching terms *)
and index_map_subnode =
| SubArray of index_array_node
| SubLeaf of term_set

(* Node that contains subnodes based on term symbol *)
and index_map_node = index_map_subnode SymbolKeyedMap.t

type term_index = {
  root: index_map_node;
}

type t = term_index

let rec string_repeat str count =
  match count with
  | 1 -> str
  | _ when 1 < count -> str ^ string_repeat str (count - 1)
  | _ -> ""

let string_of_term_index (term_index: term_index) =
  let indent_unit = "    " in
  let rec rec_array (current: index_array_node) (depth: int) =
    let indent = string_repeat indent_unit depth in
    let indent_plus = indent ^ indent_unit in
    (
      if (SparseArray.cardinal current) = 0 then
        "[]"
      else (
        "[\n" ^
        let f kvp =
          let (i, map_node) = kvp in
          Printf.sprintf "%s%d: %s" indent_plus i (rec_map map_node (depth + 1))
        in
        let current_seq: ((int * index_map_node) Seq.t) = SparseArray.to_seq current in
        let subnode_strings = List.of_seq (Seq.map f current_seq) in
        let concatenated = String.concat ",\n" subnode_strings in
        concatenated ^ "\n" ^
        indent ^ "]"
      )
    )
  and rec_map (current: index_map_node) (depth: int) =
    let indent = string_repeat indent_unit depth in
    let indent_plus = indent ^ indent_unit in
      (if (SymbolKeyedMap.cardinal current) = 0 then
        "{}"
      else (
        "{\n" ^
        let f kvp =
          let (symbol, subnode) = kvp in
          let symbol_str = Term_symbol.string_of symbol in
          let last_str = match subnode with
          | SubArray array_node -> rec_array array_node (depth + 1)
          | SubLeaf term_set -> string_of_term_set term_set
          in
          Printf.sprintf "%s%s: %s" indent_plus symbol_str last_str in
        let kvp_sequence: (Term_symbol.t * index_map_subnode) Seq.t = SymbolKeyedMap.to_seq current in
        let subnode_strings = List.of_seq (Seq.map f kvp_sequence) in
        let concatenated = String.concat ",\n" subnode_strings in
        concatenated ^ "\n" ^
        indent ^ "}"
      )
    )
  in
  rec_map term_index.root 0

let debug_string_of_option (string_of: 'a -> string) (x: 'a option) =
  match x with
  | Some value -> "Some(" ^ (string_of value) ^ ")"
  | None -> "None"

let string_of_option_variant (x: 'a option) =
  match x with
  | Some value -> "Some"
  | None -> "None"

let term_index_empty = { root = SymbolKeyedMap.empty }

let term_index_add (term_index: term_index) (pstring: Pstring.t) (term: Factory.term) =
  let rec add_map (node: index_map_node) (pstring_i: int) =
    (* Printf.printf "> add_map %d\n" pstring_i; *)
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_symbol: Term_symbol.t = (Array.get pstring pstring_i).symbol in
    (* Printf.printf ">> target_symbol: %s\n" (Pstring.string_of_term_symbol target_symbol); *)
    let pstring_at_end = (pstring_i = (Array.length pstring) - 1) in
    (* Printf.printf ">> pstring_at_end: %s\n" (if pstring_at_end then "true" else "false"); *)
    let update_symbol_value search =
      (* Printf.printf ">>> update_symbol_value: %s\n" (string_of_option_variant search); *)
      match search with
      | None -> (
        (* Create the subnode and add it to the hash table *)
        if pstring_at_end then (
          Some (SubLeaf (TermSet.singleton term))
        ) else (
          let new_subarray = add_array SparseArray.empty (pstring_i + 1) in
          Some (SubArray new_subarray)
        )
      )
      | Some subnode -> (
        (* If subarray and pstring at end -> does not make sense:
        subarray implies there are arguments to provide no matter the term
        If subarray and pstring not at end -> add_array
        If subleaf and pstring at end -> add to term set
        If subleaf and pstring not at end -> does not make sense:
          leaf implies the path ends here no matter the term *)
        match subnode with
        | SubArray subarray -> (
          assert (not pstring_at_end);
          Some (SubArray (add_array subarray (pstring_i + 1)))
        )
        | SubLeaf term_set -> (
          assert (pstring_at_end);
          Some (SubLeaf (TermSet.add term term_set))
        )
      )
    in

    SymbolKeyedMap.update target_symbol update_symbol_value node

  and add_array (node: index_array_node) (pstring_i: int) =
    (* Printf.printf "> add_array %d\n" pstring_i; *)
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_i: int = (Array.get pstring pstring_i).index in
    (* Printf.printf ">> target_i %d\n" target_i; *)
    (*
      If target exists -> add_map at that map
      If target does not exist -> insert at target_i an empty map and add_map
    *)
    let update_index_value search = match search with
    | None -> (
      let new_submap = add_map SymbolKeyedMap.empty pstring_i in
      Some new_submap
    )
    | Some found -> Some (add_map found pstring_i)
    in
    SparseArray.update target_i update_index_value node
  in
  { term_index with root = add_map term_index.root 0 }

let term_index_remove (term_index: term_index) (pstring: Pstring.t) (term: Factory.term) =
  let rec remove_map (node: index_map_node) (pstring_i: int) =
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_symbol: Term_symbol.t = (Array.get pstring pstring_i).symbol in
    let pstring_at_end = (pstring_i = (Array.length pstring) - 1) in
    let update_symbol_value search = match search with
    | None -> assert (false); (* Nothing to remove *)
    | Some subnode -> (
      (* If array, assert pstring not at end, remove_array of the subarray *)
      (* If leaf, assert pstring at end, remove from set, possibly deleting the leaf if becomes empty *)
      match subnode with
      | SubArray subarray -> (
        assert (not pstring_at_end);
        let subarray_removed = remove_array subarray (pstring_i + 1) in
        if (SparseArray.cardinal subarray_removed) = 0 then
          None (* Remove entry for target_symbol *)
        else
          Some (SubArray subarray_removed)
      )
      | SubLeaf term_set -> (
        assert (pstring_at_end);
        assert (TermSet.mem term term_set); (* term is present *)
        let term_set_removed = TermSet.remove term term_set in
        assert (not (TermSet.mem term term_set_removed)); (* term is removed *)
        if (TermSet.cardinal term_set_removed) = 0 then
          None (* Remove entry for target_symbol *)
        else
          Some (SubLeaf term_set_removed)
      )
    ) in
    SymbolKeyedMap.update target_symbol update_symbol_value node

  and remove_array (node: index_array_node) (pstring_i: int) =
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_i: int = (Array.get pstring pstring_i).index in
    let update_index_value search = match search with
    | None -> assert (false);
    | Some subnode -> (
      let subnode_removed = remove_map subnode pstring_i in
      if (SymbolKeyedMap.cardinal subnode_removed) = 0 then
        None (* Remove entry for target_i *)
      else
        Some subnode_removed
    ) in
    SparseArray.update target_i update_index_value node
  in
  { term_index with root = (remove_map term_index.root 0) }

type index_fold = {
  index : term_index;
  term : Factory.term;
}
