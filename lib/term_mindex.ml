(* Mutable implementation of a term index *)

module IndexLeafTermSet = Hashtbl.Make(
  struct
    (* Key type *)
    type t = Factory.term
    let equal = Factory.term_equal
    let hash = Factory.address_of
  end
)

type term_set = unit IndexLeafTermSet.t

let term_set_add (term_set: term_set) (term: Factory.term) =
  IndexLeafTermSet.replace term_set term ();
  ()

let term_set_remove (term_set: term_set) (term: Factory.term) =
  IndexLeafTermSet.remove term_set term;
  ()

let term_set_singleton ?(capacity=8) (term: Factory.term) =
  let created_term_set = IndexLeafTermSet.create capacity in
  IndexLeafTermSet.add created_term_set term ();
  created_term_set

let term_set_of_list (terms: Factory.term list) =
  let add_unit term = (term, ()) in
  IndexLeafTermSet.of_seq (List.to_seq (List.map add_unit terms))

let string_of_term_set (term_set: term_set) =
  let term_list = List.of_seq (IndexLeafTermSet.to_seq_keys term_set) in
  let string_of_term term = Factory.string_address_of term in
  let strings = List.map string_of_term term_list in
  Printf.sprintf "{ %s }" (String.concat ", " strings)

(* Node that contains subnodes based on argument position *)
type index_array_node = (int, index_map_node) Hashtbl.t

(* Subnode of map node: either a sub array or a leaf containing the set of matching terms *)
and index_map_subnode =
| SubArray of index_array_node
| SubLeaf of term_set

(* Node that contains subnodes based on term symbol *)
and index_map_node = (term_symbol, index_map_subnode) Hashtbl.t

type term_index = {
  root: index_map_node;
}

let index_array_node_create ?(capacity=8) () = Hashtbl.create capacity

let index_array_node_singleton ?(capacity=8) (index: int) (map_node: index_map_node) =
  let created_node = index_array_node_create ~capacity () in
  Hashtbl.add created_node index map_node;
  created_node

let index_array_node_replace (container: index_array_node) (index: int) (map_node: index_map_node) =
  Hashtbl.replace container index map_node;
  ()

let index_array_node_remove (container: index_array_node) (index: int) =
  Hashtbl.remove container index;
  ()

let index_array_node_make (index_map_nodes: index_map_node list) =
  let pair_with_index i node = (i, node) in
  Hashtbl.of_seq (List.to_seq (List.mapi pair_with_index index_map_nodes))

let index_map_node_create ?(capacity=8) () = Hashtbl.create capacity

let index_map_node_remove (container: index_map_node) (symbol: term_symbol) =
  Hashtbl.remove container symbol;
  ()

let string_of_term_index (term_index: term_index) =
  let rec rec_array (current: index_array_node) (depth: int) =
    let indent = Utils.string_repeat "  " depth in
    let indent_plus = indent ^ "  " in
    (
      if (Hashtbl.length current) = 0 then
        "[]"
      else (
        "[\n" ^
        let f kvp =
          let (i, map_node) = kvp in
          Printf.sprintf "%s%d: %s" indent_plus i (rec_map map_node (depth + 1))
        in
        let current_seq: ((int * index_map_node) Seq.t) = Hashtbl.to_seq current in
        let subnode_strings = List.of_seq (Seq.map f current_seq) in
        let concatenated = String.concat ",\n" subnode_strings in
        concatenated ^ "\n" ^
        indent ^ "]"
      )
    )
  and rec_map (current: index_map_node) (depth: int) =
    let indent = Utils.string_repeat "  " depth in
    let indent_plus = indent ^ "  " in
      (if (Hashtbl.length current) = 0 then
        "{}"
      else (
        "{\n" ^
        let f kvp =
          let (symbol, subnode) = kvp in
          let symbol_str = string_of_term_symbol symbol in
          let last_str = match subnode with
          | SubArray array_node -> rec_array array_node (depth + 1)
          | SubLeaf term_set -> string_of_term_set term_set
          in
          Printf.sprintf "%s%s: %s" indent_plus symbol_str last_str in
        let kvp_sequence: (term_symbol * index_map_subnode) Seq.t = Hashtbl.to_seq current in
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

let term_index_create ?(capacity=8) () =
  { root = Hashtbl.create capacity }

let term_index_insert (term_index: term_index) (pstring: t) (term: Factory.term) =
  let rec insert_map (node: index_map_node) (pstring_i: int) =
    (* Printf.printf "> insert_map %d\n" pstring_i; *)
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_symbol: term_symbol = (Array.get pstring pstring_i).symbol in
    (* Printf.printf ">> target_symbol: %s\n" (string_of_term_symbol target_symbol); *)
    let found_subnode = Hashtbl.find_opt node target_symbol in
    (* Printf.printf ">> found_subnode: %s\n" *)
      (* (debug_string_of_option (fun (node) -> match node with | SubArray _ -> "SubArray" | SubLeaf _ -> "SubLeaf") found_subnode); *)
    let pstring_at_end = (pstring_i = (Array.length pstring) - 1) in
    (* Printf.printf ">> pstring_at_end: %s\n" (if pstring_at_end then "true" else "false"); *)
    match found_subnode with
    | None -> (
      (* Create the subnode and add it to the hash table *)
      if pstring_at_end then (
        let new_subnode = SubLeaf (term_set_singleton term) in
        Hashtbl.add node target_symbol new_subnode;
        ()
      ) else (
        let new_subarray = index_array_node_create () in
        let new_subnode = SubArray (new_subarray) in
        Hashtbl.add node target_symbol new_subnode;
        (* Printf.printf ">> new_subarray\n"; *)
        insert_array new_subarray (pstring_i + 1)
      )
    )
    | Some subnode -> (
      (* If subarray and pstring at end -> does not make sense:
      subarray implies there are arguments to provide no matter the term
      If subarray and pstring not at end -> insert_array
      If subleaf and pstring at end -> add to term set
      If subleaf and pstring not at end -> does not make sense:
        leaf implies the path ends here no matter the term *)
      match subnode with
      | SubArray subarray -> (
        assert (not pstring_at_end);
        insert_array subarray (pstring_i + 1)
      )
      | SubLeaf term_set -> (
        assert (pstring_at_end);
        term_set_add term_set term;
        ()
      )
    )
  and insert_array (node: index_array_node) (pstring_i: int) =
    (* Printf.printf "> insert_array %d\n" pstring_i; *)
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_i: int = (Array.get pstring pstring_i).index in
    (* Printf.printf ">> target_i %d\n" target_i; *)
    (*
      If target exists -> insert_map at that map
      If target does not exist -> insert at target_i an empty map and insert_map
    *)
    let target_search = Hashtbl.find_opt node target_i in
    (* let target_exists = match target_search with Some _ -> true | None -> false in *)
    (* Printf.printf ">> target_exists %s\n" (if target_exists then "true" else "false"); *)
    match target_search with
    | Some target -> insert_map target pstring_i
    | None -> (
      let new_submap = index_map_node_create () in
      index_array_node_replace node target_i new_submap;
      insert_map new_submap pstring_i
    )
  in
  insert_map term_index.root 0

let term_index_remove (term_index: term_index) (pstring: t) (term: Factory.term) =
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
        assert (IndexLeafTermSet.mem term_set term); (* term is present *)
        term_set_remove term_set term;
        assert (not (IndexLeafTermSet.mem term_set term)); (* term is removed *)
        if (IndexLeafTermSet.length term_set) = 0 then
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
    let () = term_index_insert procedural_index pstring term in
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
