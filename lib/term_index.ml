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
  type t = Pstring.term_symbol
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

let rec string_repeat str count =
  match count with
  | 1 -> str
  | _ when 1 < count -> str ^ string_repeat str (count - 1)
  | _ -> ""

let string_of_term_index (term_index: term_index) =
  let rec rec_array (current: index_array_node) (depth: int) =
    let indent = string_repeat "  " depth in
    let indent_plus = indent ^ "  " in
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
    let indent = string_repeat "  " depth in
    let indent_plus = indent ^ "  " in
      (if (SymbolKeyedMap.cardinal current) = 0 then
        "{}"
      else (
        "{\n" ^
        let f kvp =
          let (symbol, subnode) = kvp in
          let symbol_str = Pstring.string_of_term_symbol symbol in
          let last_str = match subnode with
          | SubArray array_node -> rec_array array_node (depth + 1)
          | SubLeaf term_set -> string_of_term_set term_set
          in
          Printf.sprintf "%s%s: %s" indent_plus symbol_str last_str in
        let kvp_sequence: (Pstring.term_symbol * index_map_subnode) Seq.t = SymbolKeyedMap.to_seq current in
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

let term_index_empty = { root = SymbolKeyedMap.empty }

let term_index_add (term_index: term_index) (pstring: Pstring.t) (term: Factory.term) =
  let rec add_map (node: index_map_node) (pstring_i: int) =
    (* Printf.printf "> add_map %d\n" pstring_i; *)
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_symbol: Pstring.term_symbol = (Array.get pstring pstring_i).symbol in
    (* Printf.printf ">> target_symbol: %s\n" (string_of_term_symbol target_symbol); *)
    let pstring_at_end = (pstring_i = (Array.length pstring) - 1) in
    (* Printf.printf ">> pstring_at_end: %s\n" (if pstring_at_end then "true" else "false"); *)
    let update_symbol_value search = match search with
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
    ) in

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
      let new_submap = add_map SymbolKeyedMap.empty target_i in
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
    let target_symbol: term_symbol = (Array.get pstring pstring_i).symbol in
    let found_subnode = Hashtbl.find_opt node target_symbol in
    let pstring_at_end = (pstring_i = (Array.length pstring) - 1) in
    match found_subnode with
    | None -> (
      (* Nothing to delete *)
      assert (false);
    )
    | Some subnode -> (
      (* If array, assert pstring not at end, remove_array of the subarray *)
      (* If leaf, assert pstring at end, remove from set, possibly deleting the leaf if becomes empty *)
      match subnode with
      | SubArray subarray -> (
        assert (not pstring_at_end);
        remove_array subarray (pstring_i + 1);
        if (Hashtbl.length subarray) = 0 then
          let _ = Hashtbl.remove node target_symbol in
          ()
        else
          ()
      )
      | SubLeaf term_set -> (
        assert (pstring_at_end);
        assert (TermSet.mem term_set term); (* term is present *)
        term_set_remove term_set term;
        assert (not (TermSet.mem term_set term)); (* term is removed *)
        if (TermSet.length term_set) = 0 then
          let length_before = Hashtbl.length node in
          Hashtbl.remove node target_symbol;
          let length_after = Hashtbl.length node in
          assert (length_after = length_before - 1); (* empty set is removed *)
          ()
        else
          ()
      )
    )
  and remove_array (node: index_array_node) (pstring_i: int) =
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_i: int = (Array.get pstring pstring_i).index in
    let target_search = Hashtbl.find_opt node target_i in
    (*
    If submap exists remove_map and if |map| = 0 remove its entry
    *)
    match target_search with
    | Some subnode -> (
      remove_map subnode pstring_i;
      if (Hashtbl.length subnode) = 0 then
        let _ = index_array_node_remove node target_i in
        ()
      else
        ()
    )
    | None -> (
      let _ = assert (false) in (* Nothing to remove *)
      ()
    )
  in
  remove_map term_index.root 0

let _ =
  let make_hashtbl (list: ('a * 'b) list) =
    let sequence: ('a * 'b) Seq.t = List.to_seq list in
    Hashtbl.of_seq sequence
  in

  let factory0 = Factory.empty in
  let (a, factory1) = Factory.create_app "a" [] factory0 in
  let (b, factory2) = Factory.create_app "b" [] factory1 in
  let (c, factory3) = Factory.create_app "c" [] factory2 in
  let (x, factory4) = Factory.create_fvar "*" factory3 in
  let (g1, factory5) = Factory.create_app "g" [a; x] factory4 in
  let (g2, factory6) = Factory.create_app "g" [x; b] factory5 in
  let (g3, factory7) = Factory.create_app "g" [a; b] factory6 in
  let (g4, factory8) = Factory.create_app "g" [x; c] factory7 in
  let (f1, factory9) = Factory.create_app "f" [g1; c] factory8 in
  let (f2, factory10) = Factory.create_app "f" [g2; x] factory9 in
  let (f3, factory11) = Factory.create_app "f" [g3; c] factory10 in
  let (f4, factory12) = Factory.create_app "f" [g4; b] factory11 in
  let (f5, factory13) = Factory.create_app "f" [x; x] factory12 in
  let terms = [f1; f2; f3; f4; f5] in
  (* Printf.printf "term: %s\n" (string_of_term a_f);
  let pstrings = make_pstrings a_f in
  Printf.printf "pstrings: { %s }\n" (
    String.concat ", " (List.map string_of_pstring pstrings)
  ); *)
  let manual_index = {
    root = make_hashtbl [
      (get_term_symbol f1,
        SubArray (
          index_array_node_make [
            make_hashtbl [
              (get_term_symbol x,
                SubLeaf (term_set_of_list [f5])
              );
              (get_term_symbol g1,
                SubArray (
                  index_array_node_make [
                    make_hashtbl [
                      (get_term_symbol x,
                        SubLeaf (term_set_of_list [f2; f4])
                      );
                      (get_term_symbol a,
                        SubLeaf (term_set_of_list [f1; f3])
                      );
                    ];
                    make_hashtbl [
                      (get_term_symbol b,
                        SubLeaf (term_set_of_list [f2; f3])
                      );
                      (get_term_symbol c,
                        SubLeaf (term_set_of_list [f4])
                      );
                      (get_term_symbol x,
                        SubLeaf (term_set_of_list [f1])
                      );
                    ];
                  ]
                )
              );
            ];
            make_hashtbl [
              (get_term_symbol b,
                SubLeaf (term_set_of_list [f4])
              );
              (get_term_symbol c,
                SubLeaf (term_set_of_list [f1; f3])
              );
              (get_term_symbol x,
                SubLeaf (term_set_of_list [f2; f5])
              );
            ];
          ];
        )
      )
    ]
  } in
  Printf.printf "manual_index: %s\n\n" (string_of_term_index manual_index);

  let procedural_index = term_index_create () in

  let print_insertions arg =
    let (pstring, term) = arg in
    Printf.printf "pstring: %s\n" (string_of_pstring pstring);
    let () = term_index_add procedural_index pstring term in
    Printf.printf "procedural_index: %s\n" (string_of_term_index procedural_index);
    ()
  in

  let print_insertions_many fterm =
    Printf.printf "\nterm: %s (%s)\n" (Factory.string_of_term fterm) (Factory.string_address_of fterm);
    let pstrings = make_pstrings fterm in
    let pstrings_with_term = List.map (fun pstr -> (pstr, fterm)) pstrings in
    List.iter print_insertions pstrings_with_term;
    Printf.printf "\n";
  in

  let print_deletions arg =
    let (pstring, term) = arg in
    Printf.printf "pstring: %s\n" (string_of_pstring pstring);
    let () = term_index_remove procedural_index pstring term in
    Printf.printf "procedural_index: %s\n" (string_of_term_index procedural_index);
    ()
  in

  let print_deletions_many fterm =
    Printf.printf "\nterm: %s (%s)\n" (Factory.string_of_term fterm) (Factory.string_address_of fterm);
    let pstrings = make_pstrings fterm in
    let pstrings_with_term = List.map (fun pstr -> (pstr, fterm)) pstrings in
    List.iter print_deletions pstrings_with_term;
    Printf.printf "\n";
  in

  List.iter print_insertions_many terms;
  Printf.printf "\n\nDeletions:\n\n";
  List.iter print_deletions_many terms;
  ;;
