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

module IdComparableTerm = struct
  (* Key type *)
  type t = Term.t
  let equal = Term.term_equal
  let hash = Hashtbl.hash
end

module TermSet = Hashtbl.Make(IdComparableTerm)

type term_set = unit TermSet.t


let term_set_add (term_set: term_set) (term: Term.t) =
  TermSet.replace term_set term ();
  ()


let term_set_remove (term_set: term_set) (term: Term.t) =
  TermSet.remove term_set term;
  ()


let term_set_singleton ?(capacity=8) (term: Term.t) =
  let created_term_set = TermSet.create capacity in
  TermSet.add created_term_set term ();
  created_term_set


let term_set_create ?(capacity=8) () =
  TermSet.create capacity


let term_set_mem (term_set: term_set) (term: Term.t) =
  TermSet.mem term_set term


let string_of_term_set (term_set: term_set) =
  let term_list = List.of_seq (TermSet.to_seq_keys term_set) in
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


(* Assumes insertion and deletion already work as intended.
This only checks that the index node corresponding to the first leaf
of `total_term` (reading from left to right) contains `total_term` *)
let contains_term (term_index: t) (total_term: Term.t) =
  let rec contains_map (node: map_node) (current_term: Term.t) = (
    let target_symbol = Term_symbol.of_term current_term in
    let search = Hashtbl.find_opt node target_symbol in
    match search with
    | None -> false
    | Some subnode -> (
      match subnode with
      | SubLeaf term_set -> TermSet.mem term_set total_term
      | SubArray subarray -> (
        match current_term with
        | Bvar _ | Fvar _ | Mvar _ | App (_, []) -> false
        | App (_, args) -> (
          assert (not (List.is_empty args));
          contains_array subarray (List.hd args)
        )
        | Bind (_, body) -> contains_array subarray body
      )
    )
  )
  and contains_array (node: array_node) (current_term: Term.t) = (
    let target_i = 0 in
    let search = Hashtbl.find_opt node target_i in
    match search with
    | None -> Printf.printf "  -> false"; false
    | Some subnode -> contains_map subnode current_term
  )
  in
  contains_map term_index.root total_term


let contains_all_terms (term_index: t) (terms: Term.t Seq.t) =
  Seq.for_all (contains_term term_index) terms


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
          if (TermSet.length term_set) = 0 then (
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


(* Must iterate over `target` *)
let term_set_inter_inplace target other =
  let existing_in_t2 key value =
    match TermSet.find_opt other key with
    | Some _ -> Some value
    | None -> None
  in
  TermSet.filter_map_inplace existing_in_t2 target


(* Must iterate over `other` *)
let term_set_union_inplace target other =
  let replace_in_t1 k v = TermSet.replace target k v in
  TermSet.iter replace_in_t1 other


let term_set_filter_copy (predicate: Term.t -> bool) (term_set: term_set) =
  let result = term_set_create () in
  let remove_by_predicate (key: Term.t) =
    if predicate key then
      TermSet.replace result key ()
    else
      ()
  in
  Seq.iter remove_by_predicate (TermSet.to_seq_keys term_set);
  result


let sequence_iter_until (f: 'a -> int -> bool) (seq: 'a Seq.t) =
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
  recursive seq 0


(* The intersection of calls to `retrieve` for each entry in `array_node` alongside `args`.
Produces a new Hashtbl *)
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
  let intersection: term_set = term_set_create () in
  let add_intersection item i =
    let continue = true in
    let stop = false in
    if i = 0 then
      let retrieved = perform_retrieve item in
      TermSet.replace_seq intersection (TermSet.to_seq retrieved);
      continue
    else (
      if (TermSet.length intersection) = 0 then
        stop
      else (
        let retrieved = perform_retrieve item in
        term_set_inter_inplace intersection retrieved;
        continue
      )
    )
  in
  sequence_iter_until add_intersection packed_seq;
  intersection


(* Union of leaves associated with substitutable term(s).
Makes a union with `target` inplace *)
let union_of_substitutable (target: term_set) (map_node: map_node) (options: Term.substitutability) =
  let get_term_set substitutable symbol =
    if not substitutable then
      None
    else (
      let search = Hashtbl.find_opt map_node symbol in
      match search with
      (* Cannot be subarray because fvar and mvar are leaves *)
      | Some (SubArray _) -> (assert false);
      | Some (SubLeaf term_set) -> Some term_set
      | _ -> None
    )
  in
  let inplace_set_union set =
    match set with
    | None -> ()
    | Some set -> term_set_union_inplace target set
  in
  let fvar_set = get_term_set options.fvar Term_symbol.SymFvar in
  let mvar_set = get_term_set options.mvar Term_symbol.SymMvar in
  inplace_set_union fvar_set;
  inplace_set_union mvar_set


(* Get all descendants of map_node that are term sets, in a sequence *)
let get_map_term_set_sequence (map_node: map_node) =
  let rec rec_map map_node =
    let map (kvp: Term_symbol.t * map_subnode) =
      let (_symbol, subnode) = kvp in
      match subnode with
      | SubArray subarray -> (
        rec_array subarray
      )
      | SubLeaf term_set -> TermSet.to_seq_keys term_set
    in
    let siblings = Seq.map map (Hashtbl.to_seq map_node) in
    Seq.concat siblings
  and rec_array array_node =
    let fold acc kvp =
      let (_index, map_subnode) = kvp in
      Seq.append acc (rec_map map_subnode)
    in
    Seq.fold_left fold Seq.empty (Hashtbl.to_seq array_node)
  in
  rec_map map_node


(* Get the union of all descendants of map_node that are term sets,
filtered at the term level with `filter`. Produces a new Hashtbl *)
let get_map_term_set_union (map_node: map_node) (filter: Term.t -> bool) =
  let term_sequence = get_map_term_set_sequence map_node in
  let filter_map term =
    if filter term then
      Some (term, ())
    else
      None
  in
  let filtered_term_sequence = Seq.filter_map filter_map term_sequence in
  TermSet.of_seq filtered_term_sequence


let retrieve_generalizations (index: t)
                             (query: Term.t)
                             (options: Term.substitutability) =
  let is_generalization _term =
    true
    (* TODO: implement the predicate: term is a generalization of query *)
  in
  let filter_generalizations_copy = term_set_filter_copy is_generalization in
  let rec retrieve (map_node: map_node) (term: Term.t) =
    (* Owned by the current scope (from copy or new creation) *)
    let base_candidate_set: term_set = (
      match Term_symbol.is_function term with
      | Some (symbol, args) -> (
        let transition_search = Hashtbl.find_opt map_node symbol in
        match transition_search with
        | Some map_subnode -> (
          match map_subnode with
          | SubLeaf term_set -> filter_generalizations_copy term_set
          | SubArray subarray -> retrievals_intersection subarray args retrieve
        )
        | None -> term_set_create ()
      )
      | None -> term_set_create ()
    ) in
    union_of_substitutable base_candidate_set map_node options;
    base_candidate_set
  in
  retrieve index.root query


let retrieve_instances (index: t)
                       (query: Term.t)
                       (options: Term.substitutability) =
  let is_instance _term =
    true
    (* TODO: implement the predicate: term is an
    instance of query (using Term.match_terms) *)
  in
  let filter_instances_copy = term_set_filter_copy is_instance in
  let rec retrieve (map_node: map_node) (term: Term.t) =
    if Term.is_substitutable term options then (
      get_map_term_set_union map_node is_instance
    ) else (
      let symbol = Term_symbol.of_term term in
      let transition_search = Hashtbl.find_opt map_node symbol in
      match transition_search with
      | Some map_subnode -> (
        match map_subnode with
        | SubLeaf term_set -> filter_instances_copy term_set
        | SubArray subarray -> (
          let subterms = Term.get_subterms term in
          (* Index contains array where term has subterms *)
          assert (0 < List.length subterms);
          retrievals_intersection subarray subterms retrieve
        )
      )
      | None -> (
        (* Unspecified by the algorithm *)
        term_set_create ()
      )
    )
  in
  retrieve index.root query


let retrieve_unifiable (index: t)
                       (query: Term.t)
                       (options: Term.substitutability) =
  let is_unifiable _term =
    (* TODO: implement the predicate: term is unifiable with query
      Probably as shown below:
      Option.is_some (Term.unify term query options)

    Note: Term.unify does not work as intended when using
      options = { fvar:false, mvar:true }
      for filtering *)
    true
  in
  let filter_unifiable = term_set_filter_copy is_unifiable in
  let rec retrieve (map_node: map_node) (term: Term.t) =
    (* Owned by the current scope (from copy or new creation) *)
    let base_candidate_set = (
      if Term.is_substitutable term options then (
        get_map_term_set_union map_node is_unifiable
      ) else (
        let symbol = Term_symbol.of_term term in
        let transition_search = Hashtbl.find_opt map_node symbol in
        match transition_search with
        | Some map_subnode -> (
          match map_subnode with
          | SubLeaf term_set -> filter_unifiable term_set
          | SubArray subarray -> (
            let subterms = Term.get_subterms term in
            (* Index contains array where term has subterms *)
            assert (0 < List.length subterms);
            retrievals_intersection subarray subterms retrieve
          )
        )
        | None -> term_set_create ()
      )
    ) in
    union_of_substitutable base_candidate_set map_node options;
    base_candidate_set
  in
  retrieve index.root query


let retrieve_variants (index: t)
                      (query: Term.t) =
  let is_variant _term =
    true
    (* TODO: implement the predicate: term is a variant of query *)
  in
  let filter_variants = term_set_filter_copy is_variant in
  let rec retrieve (map_node: map_node) (term: Term.t) = (
    let symbol = Term_symbol.of_term term in
    let transition_search = Hashtbl.find_opt map_node symbol in
    match transition_search with
    | Some map_subnode -> (
      match map_subnode with
      | SubLeaf term_set -> filter_variants term_set
      | SubArray subarray -> (
        let subterms = Term.get_subterms term in
        (* Index contains array where term has subterms *)
        assert (0 < List.length subterms);
        retrievals_intersection subarray subterms retrieve
      )
    )
    | None -> (
      (* Unspecified by the algorithm *)
      term_set_create ()
    )
  ) in
  retrieve index.root query
