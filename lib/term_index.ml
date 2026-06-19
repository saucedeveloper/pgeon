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

type t = {
  root: index_map_node;
}

let string_of_term_set (term_set: term_set) =
  let term_list = TermSet.to_list term_set in
  let string_of_term term = Factory.string_address_of term in
  let strings = List.map string_of_term term_list in
  Printf.sprintf "{ %s }" (String.concat ", " strings)

let string_of ?(indent_pattern="    ") ?(indent_level=0) (term_index: t) =
  let rec rec_array (current: index_array_node) (depth: int) =
    let indent = Utils.string_repeat indent_pattern depth in
    let indent_plus = indent ^ indent_pattern in
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
    let indent = Utils.string_repeat indent_pattern depth in
    let indent_plus = indent ^ indent_pattern in
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
  rec_map term_index.root indent_level

let debug_string_of_option (string_of: 'a -> string) (x: 'a option) =
  match x with
  | Some value -> "Some(" ^ (string_of value) ^ ")"
  | None -> "None"

let string_of_option_variant (x: 'a option) =
  match x with
  | Some value -> "Some"
  | None -> "None"

let empty = { root = SymbolKeyedMap.empty }

let add (term_index: t) (pstring: Pstring.t) (term: Factory.term) =
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
        (* Create the subnode and add it to the map *)
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
    let update_index_value search =
      let map_node = Option.value search ~default:SymbolKeyedMap.empty in
      Some (add_map map_node pstring_i)
    in
    SparseArray.update target_i update_index_value node
  in
  assert (0 < Array.length pstring); (* Pstring is empty *)
  (* The first pstring symbol does not match its term's root symbol *)
  assert ((Array.get pstring 0).symbol = Term_symbol.of_term term);
  { term_index with root = add_map term_index.root 0 }

let remove (term_index: t) (pstring: Pstring.t) (term: Factory.term) =
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
    | None -> assert (false); (* Nothing to remove *)
    | Some subnode -> (
      let subnode_removed = remove_map subnode pstring_i in
      if (SymbolKeyedMap.cardinal subnode_removed) = 0 then
        None (* Remove entry for target_i *)
      else
        Some subnode_removed
    ) in
    SparseArray.update target_i update_index_value node
  in
  assert (0 < Array.length pstring); (* Pstring is empty *)
  (* The first pstring symbol does not match its term's root symbol *)
  assert ((Array.get pstring 0).symbol = Term_symbol.of_term term);
  { term_index with root = (remove_map term_index.root 0) }

let add_term (term_index: t) (total_term: Factory.term) =
  let rec add_map (node: index_map_node) (current_term: Factory.term) = (
    let target_symbol = Term_symbol.of_term current_term in

    let add_app subarray args =
      let fold_f arr arg_i =
        let (index, subterm) = arg_i in
        let array_with_added = add_array arr subterm index in
        array_with_added
      in
      let args_i = Seq.mapi (fun i x -> (i, x)) (List.to_seq args) in
      let array_with_added = Seq.fold_left fold_f subarray args_i in
      Some (SubArray array_with_added)
    in

    let update_symbol_value search = match search with
    | None -> (
      match current_term with
      | Bvar _ | Fvar _ | Mvar _ | App (_, []) -> (
        (* New leaf with term *)
        Some (SubLeaf (TermSet.singleton total_term))
      )
      | App (name, args) -> (
        (* Array with args for each index *)
        add_app SparseArray.empty args
      )
      | Bind (name, arg) -> (
        let array_with_added = add_array SparseArray.empty arg 0 in
        Some (SubArray array_with_added)
      )
    )
    | Some subnode -> (
      match subnode with
      | SubArray subarray -> (
        match current_term with
        | Bvar _ | Fvar _ | Mvar _ | App (_, []) ->
          (assert (false);) (* Cannot be leaf when array exists *)
        | App (name, args) -> (
          add_app subarray args
        )
        | Bind (name, arg) -> (
          let array_with_added = add_array subarray arg 0 in
          Some (SubArray array_with_added)
        )
      )
      | SubLeaf term_set -> (
        match current_term with
        | Bvar _ | Fvar _ | Mvar _ | App (_, []) -> (
          Some (SubLeaf (TermSet.add total_term term_set))
        )
        | App (_, _) | Bind (_, _) ->
          (assert (false);) (* Cannot be array when leaf exists *)
      )
    ) in
    SymbolKeyedMap.update target_symbol update_symbol_value node
  )
  and add_array (node: index_array_node) (current_term: Factory.term) (target_i: int) = (
    let update_index_value search = (
      let map_node = Option.value search ~default: SymbolKeyedMap.empty in
      Some (add_map map_node current_term)
    ) in
    SparseArray.update target_i update_index_value node
  )
  in
  { term_index with root = (add_map term_index.root total_term) }

let remove_term (term_index: t) (total_term: Factory.term) =
  let rec remove_map (node: index_map_node) (current_term: Factory.term) = (
    let target_symbol = Term_symbol.of_term current_term in
    let update_symbol_value search = match search with
    | None -> (assert (false);) (* Nothing to remove *)
    | Some subnode -> (
      match subnode with
      | SubArray subarray -> (
        match current_term with
        | Bvar _ | Fvar _ | Mvar _ | App (_, []) ->
          (assert (false);) (* Cannot be leaf when array exists *)
        | App (name, args) -> (
          let fold_f arr arg_i =
            let (index, subterm) = arg_i in
            let array_with_removed = remove_array arr subterm index in
            assert (
              let arity = List.length args in
              let remaining_cardinal = SparseArray.cardinal array_with_removed in
              not ((remaining_cardinal = 0) && (index < arity - 1))
            ); (* Array becomes empty before the end of removal (arity disparity) *)
            array_with_removed
          in
          let args_i = Seq.mapi (fun i x -> (i, x)) (List.to_seq args) in
          let array_with_removed = Seq.fold_left fold_f subarray args_i in
          if (SparseArray.cardinal array_with_removed) = 0 then
            None
          else
            Some (SubArray array_with_removed)
        )
        | Bind (name, arg) -> (
          let array_with_removed = remove_array subarray arg 0 in
          if (SparseArray.cardinal array_with_removed) = 0 then
            None
          else
            Some (SubArray array_with_removed)
        )
      )
      | SubLeaf term_set -> (
        match current_term with
        | Bvar _ | Fvar _ | Mvar _ | App (_, []) -> (
          assert (TermSet.mem total_term term_set); (* term is present *)
          let term_set_removed = TermSet.remove total_term term_set in
          assert (not (TermSet.mem total_term term_set_removed)); (* term is removed *)
          if (TermSet.cardinal term_set_removed) = 0 then
            None (* Remove entry for target_symbol *)
          else
            Some (SubLeaf term_set_removed)
        )
        | App (_, _) | Bind (_, _) ->
          (assert (false);) (* Cannot be array when leaf exists *)
      )
    ) in
    SymbolKeyedMap.update target_symbol update_symbol_value node
  )
  and remove_array (node: index_array_node) (current_term: Factory.term) (target_i: int) = (
    let update_index_value search = (
      match search with
      | None -> assert (false); (* Nothing to remove *)
      | Some subnode -> (
        let subnode_removed = remove_map subnode current_term in
        if (SymbolKeyedMap.cardinal subnode_removed) = 0 then
          None (* Remove entry for target_i *)
        else
          Some subnode_removed
      )
    ) in
    SparseArray.update target_i update_index_value node
  ) in
  { term_index with root = (remove_map term_index.root total_term) }
