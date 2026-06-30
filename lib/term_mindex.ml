(* Mutable implementation of a term index *)

module IndexLeafTermSet = Hashtbl.Make(
  struct
    (* Key type *)
    type t = Term.t
    let equal = Term.term_equal
    let hash = Hashtbl.hash
  end
)

type term_set = unit IndexLeafTermSet.t

let term_set_add (term_set: term_set) (term: Term.t) =
  IndexLeafTermSet.replace term_set term ();
  ()

let term_set_remove (term_set: term_set) (term: Term.t) =
  IndexLeafTermSet.remove term_set term;
  ()

let term_set_singleton ?(capacity=8) (term: Term.t) =
  let created_term_set = IndexLeafTermSet.create capacity in
  IndexLeafTermSet.add created_term_set term ();
  created_term_set

let term_set_of_list (terms: Term.t list) =
  let add_unit term = (term, ()) in
  IndexLeafTermSet.of_seq (List.to_seq (List.map add_unit terms))

let term_set_mem (term_set: term_set) (term: Term.t) =
  IndexLeafTermSet.mem term_set term

let string_of_term_set (term_set: term_set) =
  let term_list = List.of_seq (IndexLeafTermSet.to_seq_keys term_set) in
  let string_of_term term = Utils.string_address_of term in
  let strings = List.map string_of_term term_list in
  Printf.sprintf "{ %s }" (String.concat ", " strings)

(* Node that contains subnodes based on argument position *)
type array_node = (int, map_node) Hashtbl.t

(* Subnode of map node: either a sub array or a leaf containing the set of matching terms *)
and map_subnode =
| SubArray of array_node
| SubLeaf of term_set

(* Node that contains subnodes based on term symbol *)
and map_node = (Term_symbol.t, map_subnode) Hashtbl.t

type t = {
  root: map_node;
}

let array_node_create ?(capacity=8) () = Hashtbl.create capacity

let array_node_replace (container: array_node) (index: int) (map_node: map_node) =
  Hashtbl.replace container index map_node;
  ()

let array_node_remove (container: array_node) (index: int) =
  Hashtbl.remove container index;
  ()

let array_node_make (map_nodes: map_node list) =
  let pair_with_index i node = (i, node) in
  Hashtbl.of_seq (List.to_seq (List.mapi pair_with_index map_nodes))

let map_node_create ?(capacity=8) () = Hashtbl.create capacity

let map_node_remove (container: map_node) (symbol: Term_symbol.t) =
  Hashtbl.remove container symbol;
  ()

let string_of ?(indent_pattern="    ") ?(indent_level=0) (term_index: t) =
  let rec rec_array (current: array_node) (depth: int) =
    let indent = Utils.string_repeat indent_pattern depth in
    let indent_plus = indent ^ indent_pattern in
    (
      if (Hashtbl.length current) = 0 then
        "[]"
      else (
        "[\n" ^
        let f kvp =
          let (i, map_node) = kvp in
          Printf.sprintf "%s%d: %s" indent_plus i (rec_map map_node (depth + 1))
        in
        let current_seq: ((int * map_node) Seq.t) = Hashtbl.to_seq current in
        let subnode_strings = List.of_seq (Seq.map f current_seq) in
        let concatenated = String.concat ",\n" subnode_strings in
        concatenated ^ "\n" ^
        indent ^ "]"
      )
    )
  and rec_map (current: map_node) (depth: int) =
    let indent = Utils.string_repeat indent_pattern depth in
    let indent_plus = indent ^ indent_pattern in
      (if (Hashtbl.length current) = 0 then
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
        let kvp_sequence: (Term_symbol.t * map_subnode) Seq.t = Hashtbl.to_seq current in
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

let create ?(capacity=8) () =
  { root = Hashtbl.create capacity }

let is_empty index = 0 = Hashtbl.length index.root

let insert_pstring (term_index: t) (pstring: Pstring.t) (term: Term.t) =
  let rec insert_map (node: map_node) (pstring_i: int) =
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_symbol: Term_symbol.t = (Array.get pstring pstring_i).symbol in
    let found_subnode = Hashtbl.find_opt node target_symbol in
    let pstring_at_end = (pstring_i = (Array.length pstring) - 1) in
    match found_subnode with
    | None -> (
      (* Create the subnode and add it to the hash table *)
      if pstring_at_end then (
        let new_subnode = SubLeaf (term_set_singleton term) in
        Hashtbl.add node target_symbol new_subnode;
        ()
      ) else (
        let new_subarray = array_node_create () in
        let new_subnode = SubArray (new_subarray) in
        Hashtbl.add node target_symbol new_subnode;
        insert_array new_subarray (pstring_i + 1);
        assert (0 < Hashtbl.length new_subarray); (* New subarray is not empty *)
        ()
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
        assert (not (term_set_mem term_set term)); (* term is not already in *)
        assert (pstring_at_end);
        assert (term_set_mem term_set term); (* term is added *)
        term_set_add term_set term;
        ()
      )
    )
  and insert_array (node: array_node) (pstring_i: int) =
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_i: int = (Array.get pstring pstring_i).index in
    (*
      If target exists -> insert_map at that map
      If target does not exist -> insert at target_i an empty map and insert_map
    *)
    let target_search = Hashtbl.find_opt node target_i in
    match target_search with
    | Some target -> insert_map target pstring_i
    | None -> (
      let new_submap = map_node_create () in
      array_node_replace node target_i new_submap;
      insert_map new_submap pstring_i;
      assert (0 < Hashtbl.length new_submap); (* New map is not empty *)
    )
  in
  insert_map term_index.root 0

let remove_pstring (term_index: t) (pstring: Pstring.t) (term: Term.t) =
  let rec remove_map (node: map_node) (pstring_i: int) =
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_symbol: Term_symbol.t = (Array.get pstring pstring_i).symbol in
    let found_subnode = Hashtbl.find_opt node target_symbol in
    let pstring_at_end = (pstring_i = (Array.length pstring) - 1) in
    match found_subnode with
    | None ->
      (assert (false);) (* Nothing to delete *)
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
        if (IndexLeafTermSet.length term_set) = 0 then (
          Hashtbl.remove node target_symbol;
          assert (not (Hashtbl.mem node target_symbol)); (* empty set is removed *)
          ()
        ) else
          ()
      )
    )
  and remove_array (node: array_node) (pstring_i: int) =
    assert (0 <= pstring_i && pstring_i < (Array.length pstring));
    let target_i: int = (Array.get pstring pstring_i).index in
    let target_search = Hashtbl.find_opt node target_i in
    (* If submap exists remove_map and if |map| = 0 remove its entry *)
    match target_search with
    | Some subnode -> (
      remove_map subnode pstring_i;
      if (Hashtbl.length subnode) = 0 then
        let _ = array_node_remove node target_i in
        ()
      else
        ()
    )
    | None -> (assert (false);)
  in
  remove_map term_index.root 0

let insert_term (term_index: t) (total_term: Term.t) =
  let rec insert_map (node: map_node) (current_term: Term.t) = (
    let target_symbol = Term_symbol.of_term current_term in
    let subnode_search = Hashtbl.find_opt node target_symbol in

    match subnode_search with
    | None -> (
      match current_term with
      | Bvar _ | Fvar _ | Mvar _ | App (_, []) -> (
        (* New leaf with term *)
        Hashtbl.add node target_symbol (SubLeaf (term_set_singleton total_term));
        ()
      )
      | App (_name, args) -> (
        (* Array with args for each index *)
        let subarray = array_node_create () in
        let subnode = SubArray subarray in
        let insert_subterm (i: int) (term: Term.t) =
          insert_array subarray term i;
          ()
        in
        List.iteri insert_subterm args;
        assert ((List.length args) = (Hashtbl.length subarray));
        Hashtbl.add node target_symbol subnode;
        ()
      )
      | Bind (_name, arg) -> (
        let subarray = array_node_create () in
        insert_array subarray arg 0;
        assert ((Hashtbl.length subarray) = 1);
        let subnode = SubArray subarray in
        Hashtbl.add node target_symbol subnode;
        ()
      )
    )
    | Some subnode -> (
      match subnode with
      | SubArray subarray -> (
        match current_term with
        | Bvar _ | Fvar _ | Mvar _ | App (_, []) ->
          (assert (false);) (* Cannot be leaf when array exists *)
        | App (_name, args) -> (
          let insert_subterm (i: int) (term: Term.t) =
            insert_array subarray term i;
            ()
          in
          List.iteri insert_subterm args;
          ()
        )
        | Bind (_name, arg) -> (
          insert_array subarray arg 0;
          ()
        )
      )
      | SubLeaf term_set -> (
        match current_term with
        | Bvar _ | Fvar _ | Mvar _ | App (_, []) -> (
          assert (not (term_set_mem term_set total_term)); (* term is not already in *)
          term_set_add term_set total_term;
          assert (term_set_mem term_set total_term); (* term is added *)
          ()
        )
        | App (_, _) | Bind (_, _) ->
          (assert (false);) (* Cannot be array when leaf exists *)
      )
    )
  )
  and insert_array (node: array_node) (current_term: Term.t) (target_i: int) = (
    let target_search = Hashtbl.find_opt node target_i in
    match target_search with
    | None -> (
      let map_node = map_node_create () in
      let length_before = Hashtbl.length map_node in
      insert_map map_node current_term;
      let length_after = Hashtbl.length map_node in
      (* If the current map is empty, the new one is not *)
      assert (not ((0 < length_before)
                && (0 < length_after)));
      Hashtbl.add node target_i map_node;
      ()
    )
    | Some subnode -> (
      insert_map subnode current_term
    )
  )
  in
  insert_map term_index.root total_term;
  ()

let remove_term (term_index: t) (total_term: Term.t) =
  let rec remove_map (node: map_node) (current_term: Term.t) = (
    let target_symbol = Term_symbol.of_term current_term in
    let subnode_search = Hashtbl.find_opt node target_symbol in

    match subnode_search with
    | None -> (assert (false);) (* Nothing to remove *)
    | Some subnode -> (
      match subnode with
      | SubArray subarray -> (
        match current_term with
        | Bvar _ | Fvar _ | Mvar _ | App (_, []) ->
          (assert (false);) (* Cannot be leaf when array exists *)
        | App (_name, args) -> (
          let remove_subterm i arg =
            remove_array subarray arg i;
            assert (
              let arity = List.length args in
              let remaining_cardinal = Hashtbl.length subarray in
              not ((remaining_cardinal = 0) && (i < arity - 1))
            ); (* Array becomes empty before the end of removal (arity disparity) *)
          in
          List.iteri remove_subterm args;
          if (Hashtbl.length subarray) = 0 then (
            map_node_remove node target_symbol;
            assert (not (Hashtbl.mem node target_symbol)); (* empty array is removed *)
            ()
          ) else
            ()
        )
        | Bind (_name, arg) -> (
          remove_array subarray arg 0;
          if (Hashtbl.length subarray) = 0 then (
            map_node_remove node target_symbol;
            assert (not (Hashtbl.mem node target_symbol)); (* empty array is removed *)
            ()
          ) else
            ()
        )
      )
      | SubLeaf term_set -> (
        match current_term with
        | Bvar _ | Fvar _ | Mvar _ | App (_, []) -> (
          assert (term_set_mem term_set total_term); (* term is present *)
          term_set_remove term_set total_term;
          assert (not (term_set_mem term_set total_term)); (* term is removed *)
          if (IndexLeafTermSet.length term_set) = 0 then (
            map_node_remove node target_symbol;
            assert (not (Hashtbl.mem node target_symbol)); (* empty set is removed *)
            ()
          ) else
            ()
        )
        | App (_, _) | Bind (_, _) ->
          (assert (false);) (* Cannot be array when leaf exists *)
      )
    )
  )
  and remove_array (node: array_node) (current_term: Term.t) (target_i: int) = (
    let target_search = Hashtbl.find_opt node target_i in
    match target_search with
    | None -> (assert (false);) (* Nothing to remove *)
    | Some subnode -> (
      remove_map subnode current_term;
      if (Hashtbl.length subnode) = 0 then (
        array_node_remove node target_i;
        assert (not (Hashtbl.mem node target_i)); (* empty map is removed *)
        ()
      ) else
        ()
    )
  ) in

  remove_map term_index.root total_term;
  ()

let get_example_index factory0 =
  let make_hashtbl (list: ('a * 'b) list) =
    let sequence: ('a * 'b) Seq.t = List.to_seq list in
    Hashtbl.of_seq sequence
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
  (* Printf.printf "term: %s\n" (string_of_term a_f);
  let pstrings = Pstring.all_of_term a_f in
  Printf.printf "pstrings: { %s }\n" (
    String.concat ", " (List.map Pstring.string_of pstrings)
  ); *)
  let manual_index = {
    root = make_hashtbl [
      (Term_symbol.of_term f1,
        SubArray (
          array_node_make [
            make_hashtbl [
              (Term_symbol.of_term x,
                SubLeaf (term_set_of_list [f5])
              );
              (Term_symbol.of_term g1,
                SubArray (
                  array_node_make [
                    make_hashtbl [
                      (Term_symbol.of_term x,
                        SubLeaf (term_set_of_list [f2; f4])
                      );
                      (Term_symbol.of_term a,
                        SubLeaf (term_set_of_list [f1; f3])
                      );
                    ];
                    make_hashtbl [
                      (Term_symbol.of_term b,
                        SubLeaf (term_set_of_list [f2; f3])
                      );
                      (Term_symbol.of_term c,
                        SubLeaf (term_set_of_list [f4])
                      );
                      (Term_symbol.of_term x,
                        SubLeaf (term_set_of_list [f1])
                      );
                    ];
                  ]
                )
              );
            ];
            make_hashtbl [
              (Term_symbol.of_term b,
                SubLeaf (term_set_of_list [f4])
              );
              (Term_symbol.of_term c,
                SubLeaf (term_set_of_list [f1; f3])
              );
              (Term_symbol.of_term x,
                SubLeaf (term_set_of_list [f2; f5])
              );
            ];
          ];
        )
      )
    ]
  } in
  (manual_index, terms, factory13)
