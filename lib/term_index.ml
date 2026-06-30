(* Immutable implementation of a term index *)

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
  type t = Term.t
  let compare a b = Stdlib.compare a b
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

let string_of_term_set ?(n=4) (term_set: term_set) =
  let term_list = TermSet.to_list term_set in
  let string_of_term term = Utils.string_address_of ~n:n term in
  let strings = List.map string_of_term term_list in
  Printf.sprintf "{ %s }" (String.concat ", " strings)

let string_of_term_set_full ?(n=4) (term_set: term_set) =
  let term_list = TermSet.to_list term_set in
  let string_of_term term = 
    if (0 < n) then
      (Term.string_of term) ^ "{@" ^ (Utils.string_address_of ~n:n term) ^ "}"
    else
      Term.string_of term
  in
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

(* let debug_string_of_option (string_of: 'a -> string) (x: 'a option) =
  match x with
  | Some value -> "Some(" ^ (string_of value) ^ ")"
  | None -> "None" *)

(* let string_of_option_variant (x: 'a option) =
  match x with
  | Some _ -> "Some"
  | None -> "None" *)

let empty = { root = SymbolKeyedMap.empty }

let index_with_root (_index: t) (root: index_map_node) =
  let result: t = { root = root } in
  result

let is_empty index = SymbolKeyedMap.is_empty index.root

let add_pstring (term_index: t) (pstring: Pstring.t) (term: Term.t) =
  let rec add_map (node: index_map_node) (pstring_i: int) =
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_symbol: Term_symbol.t = (Array.get pstring pstring_i).symbol in
    let pstring_at_end = (pstring_i = (Array.length pstring) - 1) in
    let update_symbol_value search =
      match search with
      | None -> (
        (* Create the subnode and add it to the map *)
        if pstring_at_end then (
          Some (SubLeaf (TermSet.singleton term))
        ) else (
          let new_subarray = add_array SparseArray.empty (pstring_i + 1) in
          (* The created sub array is not empty *)
          assert (not (SparseArray.is_empty new_subarray));
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
          let new_subarray = add_array subarray (pstring_i + 1) in
          Some (SubArray new_subarray)
        )
        | SubLeaf term_set -> (
          assert (pstring_at_end);
          assert (not (TermSet.mem term term_set)); (* term is not already in *)
          let new_term_set = TermSet.add term term_set in
          assert (TermSet.mem term new_term_set); (* term is added *)
          Some (SubLeaf new_term_set)
        )
      )
    in
    SymbolKeyedMap.update target_symbol update_symbol_value node

  and add_array (node: index_array_node) (pstring_i: int) =
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_i: int = (Array.get pstring pstring_i).index in
    (*
      If target exists -> add_map at that map
      If target does not exist -> insert at target_i an empty map and add_map
    *)
    let update_index_value search =
      let map_node = Option.value search ~default:SymbolKeyedMap.empty in
      let new_submap = add_map map_node pstring_i in
      (* If the current map is empty, the new one is not *)
      assert (not ((SymbolKeyedMap.is_empty map_node)
                && (SymbolKeyedMap.is_empty new_submap)));
      Some new_submap
    in
    SparseArray.update target_i update_index_value node
  in
  assert (0 < Array.length pstring); (* Pstring is not empty *)
  (* The first pstring symbol matches its term's root symbol *)
  assert ((Array.get pstring 0).symbol = Term_symbol.of_term term);
  index_with_root term_index (add_map term_index.root 0)

let remove_pstring (term_index: t) (pstring: Pstring.t) (term: Term.t) =
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
  index_with_root term_index (remove_map term_index.root 0)

let add_term (term_index: t) (total_term: Term.t) =
  let rec add_map (node: index_map_node) (current_term: Term.t) = (
    let target_symbol = Term_symbol.of_term current_term in

    let add_app subarray args =
      let fold_f arr arg_i =
        let (index, subterm) = arg_i in
        let array_with_added = add_array arr subterm index in
        array_with_added
      in
      let args_i = Seq.mapi (fun i x -> (i, x)) (List.to_seq args) in
      let array_with_added = Seq.fold_left fold_f subarray args_i in
      (* If the current array is empty, the new one is not *)
      assert (not ((SparseArray.is_empty subarray)
                && (SparseArray.is_empty array_with_added)));
      Some (SubArray array_with_added)
    in

    let update_symbol_value search = match search with
    | None -> (
      match current_term with
      | Bvar _ | Fvar _ | Mvar _ | App (_, []) -> (
        (* New leaf with term *)
        Some (SubLeaf (TermSet.singleton total_term))
      )
      | App (_name, args) -> (
        (* Array with args for each index *)
        add_app SparseArray.empty args
      )
      | Bind (_name, arg) -> (
        (* Array with arg for index 0 *)
        let array_with_added = add_array SparseArray.empty arg 0 in
        (* The created sub array is not empty *)
        assert (not (SparseArray.is_empty array_with_added));
        Some (SubArray array_with_added)
      )
    )
    | Some subnode -> (
      match subnode with
      | SubArray subarray -> (
        match current_term with
        | Bvar _ | Fvar _ | Mvar _ | App (_, []) ->
          (assert (false);) (* Cannot be leaf when array exists *)
        | App (_name, args) -> (
          add_app subarray args
        )
        | Bind (_name, arg) -> (
          let array_with_added = add_array subarray arg 0 in
          Some (SubArray array_with_added)
        )
      )
      | SubLeaf term_set -> (
        match current_term with
        | Bvar _ | Fvar _ | Mvar _ | App (_, []) -> (
          assert (not (TermSet.mem total_term term_set)); (* term is not already in *)
          let set_with_added = TermSet.add total_term term_set in
          assert (TermSet.mem total_term set_with_added); (* term is added *)
          Some (SubLeaf set_with_added)
        )
        | App (_, _) | Bind (_, _) ->
          (assert (false);) (* Cannot be array when leaf exists *)
      )
    ) in
    SymbolKeyedMap.update target_symbol update_symbol_value node
  )
  and add_array (node: index_array_node) (current_term: Term.t) (target_i: int) = (
    let update_index_value search = (
      let map_node = Option.value search ~default: SymbolKeyedMap.empty in
      let new_submap = add_map map_node current_term in
      (* If the current map is empty, the new one is not *)
      assert (not ((SymbolKeyedMap.is_empty map_node)
                && (SymbolKeyedMap.is_empty new_submap)));
      Some new_submap
    ) in
    SparseArray.update target_i update_index_value node
  )
  in
  index_with_root term_index (add_map term_index.root total_term)

let remove_term (term_index: t) (total_term: Term.t) =
  let rec remove_map (node: index_map_node) (current_term: Term.t) = (
    let target_symbol = Term_symbol.of_term current_term in
    let update_symbol_value search = match search with
    | None -> (assert (false);) (* Nothing to remove *)
    | Some subnode -> (
      match subnode with
      | SubArray subarray -> (
        assert (not (SparseArray.is_empty subarray));
        match current_term with
        | Bvar _ | Fvar _ | Mvar _ | App (_, []) ->
          (assert (false);) (* Cannot be leaf when array exists *)
        | App (_name, args) -> (
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
        | Bind (_name, arg) -> (
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
  and remove_array (node: index_array_node) (current_term: Term.t) (target_i: int) = (
    let update_index_value search = (
      match search with
      | None -> assert (false); (* Nothing to remove *)
      | Some subnode -> (
        assert (not (SymbolKeyedMap.is_empty subnode));
        let subnode_removed = remove_map subnode current_term in
        if (SymbolKeyedMap.cardinal subnode_removed) = 0 then
          None (* Remove entry for target_i *)
        else
          Some subnode_removed
      )
    ) in
    SparseArray.update target_i update_index_value node
  ) in
  index_with_root term_index (remove_map term_index.root total_term)

let retreive_generalizations (index: t)
                             (total_term: Term.t)
                             ~(fvar_instanciable: bool)
                             ~(mvar_instanciable: bool) =
  let rec retreive (map_node: index_map_node) (term: Term.t) =
    let term_is_function =
      let open Term in
      let open Term_symbol in
        match term with
        | Bvar _ | Fvar _ | Mvar _ | App (_, []) -> None
        | App (name, args) -> Some (SymApp, name, args)
        | Bind (name, arg) -> Some (SymBind, name, [arg])
    in
    let first_candidate_set: term_set = (
      match term_is_function with
      | Some (variant, name, args) -> (
        let term_symbol: Term_symbol.t = {
          variant = variant;
          name = name
        } in
        let transition_search = SymbolKeyedMap.find_opt term_symbol map_node in
        match transition_search with
        | Some map_subnode -> (
          match map_subnode with
          | SubLeaf term_set -> term_set
          | SubArray subarray -> (
            let union_retreive term_union value =
              let (arg, map_node) = value in
              let retreived = retreive map_node arg in
              TermSet.union retreived term_union
            in

            (* The array in the index contains as many subnodes as
            the term being represented contains arguments *)
            assert ((List.length args) = (SparseArray.cardinal subarray));

            let args_seq: Term.t Seq.t = List.to_seq args in
            let subarray_seq: (int * index_map_node) Seq.t = SparseArray.to_seq subarray in
            let pack (arg: Term.t) (array_kvp: int * index_map_node) =
              let (_i, map_node) = array_kvp in
              (arg, map_node)
            in
            let packed_seq: (Term.t * index_map_node) Seq.t =
              Seq.map2 pack args_seq subarray_seq
            in
            let term_union = Seq.fold_left union_retreive TermSet.empty packed_seq in
            term_union
          )
        )
        | None -> TermSet.empty
      )
      | None -> TermSet.empty
    ) in
    let second_candidate_set = (
      let variant_is_instanciable (variant: Term_symbol.variant) =
        let open Term_symbol in
          match variant with
          | SymFvar -> fvar_instanciable
          | SymMvar -> mvar_instanciable
          | _ -> false
      in
      let map_node_seq: (Term_symbol.t * index_map_subnode) Seq.t =
        SymbolKeyedMap.to_seq map_node
      in
      let instanciable_sets: (term_set Seq.t) =
        let predicate ((symbol, subnode): Term_symbol.t * index_map_subnode) =
          match subnode with
          | SubArray _ -> None
          | SubLeaf term_set -> (
            if (variant_is_instanciable symbol.variant) then
              Some term_set
            else
              None
          )
        in
        Seq.filter_map predicate map_node_seq
      in
      let union (term_union: term_set) (current_set: term_set) =
        TermSet.union term_union current_set
      in
      let term_union = Seq.fold_left union TermSet.empty instanciable_sets in
      term_union
    ) in
    TermSet.union first_candidate_set second_candidate_set
  in
  retreive index.root total_term

let get_example_index factory0 =
  let make_map (list: ('a * 'b) list) =
    let sequence: ('a * 'b) Seq.t = List.to_seq list in
    SymbolKeyedMap.of_seq sequence
  in

  let index_array_node_make (nodes: index_map_node list) =
    let pair i x = (i, x) in
    let sequence = Seq.mapi pair (List.to_seq nodes) in
    SparseArray.of_seq sequence
  in

  let (a, factory1) = Term.create_app "a" [] factory0 in
  let (b, factory2) = Term.create_app "b" [] factory1 in
  let (c, factory3) = Term.create_app "c" [] factory2 in
  let (x, factory4) = Term.create_fvar "*" factory3 in
  let (g1, factory5) = Term.create_app "g" [a; x] factory4 in
  let (g2, factory6) = Term.create_app "g" [x; b] factory5 in
  let (g3, factory7) = Term.create_app "g" [a; b] factory6 in
  let (g4, factory8) = Term.create_app "g" [x; c] factory7 in
  let (f1, factory9) = Term.create_app "f" [g1; c] factory8 in
  let (f2, factory10) = Term.create_app "f" [g2; x] factory9 in
  let (f3, factory11) = Term.create_app "f" [g3; c] factory10 in
  let (f4, factory12) = Term.create_app "f" [g4; b] factory11 in
  let (f5, factory13) = Term.create_app "f" [x; x] factory12 in
  let terms = [f1; f2; f3; f4; f5] in

  let manual_index = {
    root = make_map [
      (Term_symbol.of_term f1,
        SubArray (
          index_array_node_make [
            make_map [
              (Term_symbol.of_term x,
                SubLeaf (TermSet.of_list [f5])
              );
              (Term_symbol.of_term g1,
                SubArray (
                  index_array_node_make [
                    make_map [
                      (Term_symbol.of_term x,
                        SubLeaf (TermSet.of_list [f2; f4])
                      );
                      (Term_symbol.of_term a,
                        SubLeaf (TermSet.of_list [f1; f3])
                      );
                    ];
                    make_map [
                      (Term_symbol.of_term b,
                        SubLeaf (TermSet.of_list [f2; f3])
                      );
                      (Term_symbol.of_term c,
                        SubLeaf (TermSet.of_list [f4])
                      );
                      (Term_symbol.of_term x,
                        SubLeaf (TermSet.of_list [f1])
                      );
                    ];
                  ]
                )
              );
            ];
            make_map [
              (Term_symbol.of_term b,
                SubLeaf (TermSet.of_list [f4])
              );
              (Term_symbol.of_term c,
                SubLeaf (TermSet.of_list [f1; f3])
              );
              (Term_symbol.of_term x,
                SubLeaf (TermSet.of_list [f2; f5])
              );
            ];
          ];
        )
      )
    ]
  } in
  (manual_index, terms, factory13)
