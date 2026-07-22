(* Mutable implementation of a term index *)

(*
Notes: insert_pstring was developped first, as it matched the
thinking explained in the Handbook of Automated Reasoning.
Then was proposed the idea of inserting terms directly
without generating path-strings, which gave birth to
insert_term. A consequence of being able to insert
path-strings one by one is that array-like nodes (states with
integer labelled transitions) must be able to contain index
keys that may not be contiguous. As a result, the Hashtbl
data structure was chosen. If path-string insertion is not
needed, then only term insertion (which can create values for
all keys at once) could remain, allowing for a change from an
associative data structure to a linear one that would benefit
strongly from constant time access, such as Array.
*)

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


(* let term_set_create ?(capacity=8) () =
  IndexLeafTermSet.create capacity *)


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


let map_node_create ?(capacity=8) () = Hashtbl.create capacity


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


let create ?(capacity=8) () =
  { root = Hashtbl.create capacity }


let is_empty index = 0 = Hashtbl.length index.root


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
            Hashtbl.remove node target_symbol;
            assert (not (Hashtbl.mem node target_symbol)); (* empty array is removed *)
            ()
          ) else
            ()
        )
        | Bind (_name, arg) -> (
          remove_array subarray arg 0;
          if (Hashtbl.length subarray) = 0 then (
            Hashtbl.remove node target_symbol;
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
            Hashtbl.remove node target_symbol;
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
        Hashtbl.remove node target_i;
        assert (not (Hashtbl.mem node target_i)); (* empty map is removed *)
        ()
      ) else
        ()
    )
  ) in

  remove_map term_index.root total_term;
  ()


let insert_terms (term_index: t) (terms: Term.t Seq.t) =
  Seq.iter (insert_term term_index) terms


let remove_terms (term_index: t) (terms: Term.t Seq.t) =
  Seq.iter (remove_term term_index) terms


(* let hashtbl_inter_inplace t1 t2 =
  let existing_in_t2 key value =
    match Hashtbl.find_opt t2 key with
    | Some _ -> Some value
    | None -> None
  in
  Hashtbl.filter_map_inplace existing_in_t2 t1


let hashtbl_union_inplace t1 t2 =
  let replace_in_t1 k v = Hashtbl.replace t1 k v in
  Hashtbl.iter replace_in_t1 t2 *)


(* let sequence_iter_until (f: 'a -> int -> bool) (seq: 'a Seq.t) =
  let rec recursive remaining depth =
    match remaining () with
    | Seq.Nil -> ()
    | Seq.Cons (current, new_remaining) -> (
      let continue = f current depth in
      if continue then
        recursive new_remaining (depth + 1)
      else
        ()
    )
  in
  recursive seq 0 *)


(* The intersection of calls to `retrieve` for each entry in `array_node` alongside `args`
let retrievals_intersection (array_node: array_node)
                            (args: Term.t list)
                            (retrieve: map_node -> Term.t -> term_set) =
  (* The array in the index contains as many subnodes as
  the term being represented contains arguments *)
  assert ((List.length args) = (Hashtbl.length array_node));
  (* Args is not empty <=> the term has subterms *)
  assert (0 < List.length args);

  let args_seq: Term.t Seq.t = List.to_seq args in
  let subarray_seq: (int * map_node) Seq.t = Hashtbl.to_seq array_node in
  let pack (arg: Term.t) (array_kvp: int * map_node) =
    let (_i, map_node) = array_kvp in
    (arg, map_node)
  in
  let packed_seq: (Term.t * map_node) Seq.t =
    Seq.map2 pack args_seq subarray_seq
  in
  let perform_retrieve (item: Term.t * map_node) =
    let (arg, map_node) = item in
    retrieve map_node arg
  in
  let intersection = term_set_create () in
  let add_intersection (item, i) =
    let continue = true in
    let stop = false in
    if i = 0 then
      let retrieved = perform_retrieve item in
      Hashtbl.replace_seq intersection (Hashtbl.to_seq retrieved);
      continue
    else (
      if (Hashtbl.length intersection) = 0 then
        stop
      else (
        let retrieved = perform_retrieve item in
        hashtbl_inter_inplace intersection retrieved;
        continue
      )
    )
  in
  sequence_iter_until add_intersection packed_seq;
  intersection *)
