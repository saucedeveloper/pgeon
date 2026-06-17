type term_symbol_variant =
| SymBvar
| SymFvar
| SymMvar
| SymApp
| SymBind

(* Identifies the term symbol uniquely *)
type term_symbol = {
  variant: term_symbol_variant;
  name: Factory.name;
}

type pstring_node = {
  symbol: term_symbol;
  index: int;
}

(* Path string: array of index/symbol pairs decribing the traversal of a term *)
type t = pstring_node array

let pstring_node_root_index = -1

let get_term_symbol term =
  match term with
  | Factory.Bvar index -> { variant = SymBvar; name = string_of_int index }
  | Factory.Fvar name -> { variant = SymFvar; name = name }
  | Factory.Mvar name -> { variant = SymMvar; name = name }
  | Factory.App (name, _) -> { variant = SymApp; name = name }
  | Factory.Bind (name, _) -> { variant = SymBind; name = name }

let list_map_index (f: 'a -> int -> 'b) (list: 'a list) =
  let rec recursive remainder index = match remainder with
    | [] -> []
    | hd::tl -> (f hd index)::(recursive tl (index + 1))
  in
  recursive list 0

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
let make_pstrings term =
  let shared_path: pstring_node Dynarray.t = Dynarray.create () in
  let rec recursive (term: Factory.term) (current_index: int) =
    let created_node = { index = current_index; symbol = get_term_symbol term } in
    Dynarray.add_last shared_path created_node;
    match term with
    | Bvar _ | Fvar _ | Mvar _ | App (_, []) -> (
      let resulting_path = Dynarray.to_array shared_path in
      Dynarray.remove_last shared_path;
      [resulting_path]
    )
    | App (name, terms) -> (
      let f index inner = recursive inner index in
      let created_paths_by_term = List.mapi f terms in
      let inner_created = List.concat created_paths_by_term in
      Dynarray.remove_last shared_path;
      inner_created
    )
    | Bind (name, inner) -> (
      let inner_created = recursive inner 0 in
      Dynarray.remove_last shared_path;
      inner_created
    )
  in
  let result = recursive term pstring_node_root_index in
  assert ((Dynarray.length shared_path) = 0);
  result

  (* (* Returns the list of created paths *)
  let rec recursive (path_total: t) (path_last: pstring_node option) (term: Factory.term) = (
    match path_total with
    | [] -> (
      let root_node = { index = pstring_node_root_index; symbol = get_term_symbol term } in
      let new_path = [root_node] in
      let created = recursive new_path (Some root_node) term in
      created
    )
    | _ -> (
      match path_last with
      | None -> assert(false);
      | Some last -> (
        let created = { index = last.index; symbol = get_term_symbol term } in
        match term with
        | Bvar _ | Fvar _ | Mvar _ -> created
        | App (name, terms) -> (
          let new_path_total = path_total @ [created] in
          let new_path_last = created in
          let created_recursively = List.map (recursive new_path_total (Some new_path_last)) terms in
          let created = [created] @ (List.concat created_recursively) in
          created
        )
        | Bind (name, term) -> (
          let new_path_total = path_total::created in
          let new_path_last = created in
          let created_recursively = recursive new_path_total (Some new_path_last) term in
          let created = created @ [created_recursively] in
          created
        )
      )
    )
    (* match term with
    | Bvar _ | Fvar _ | Mvar _ -> [current_path]
    | App (name, terms) -> (
      let get_inner_paths inner index =
        let updated_path = current_path @ [index] in
        recursive inner updated_path paths
      in
      let inner_paths = list_map_index get_inner_paths terms in
      List.concat inner_paths
    )
    | Bind (name, inner) -> (
      let updated_path = current_path @ [0] in
      recursive inner updated_path paths
    ) *)
  ) in
  recursive [] None term *)

let get_subterm term index =
  match term with
  | Factory.Bvar _ | Factory.Fvar _ | Factory.Mvar _ -> None
  | Factory.App (name, terms) -> (
    List.nth_opt terms index
  )
  | Factory.Bind (name, term) -> (
    if index = 0 then (Some term) else None
  )

let string_of_term_symbol (symbol: term_symbol) =
  match symbol.variant with
  | SymBvar -> "#" ^ symbol.name
  | SymFvar -> "'" ^ symbol.name
  | SymMvar -> "?" ^ symbol.name
  | SymApp  -> ""  ^ symbol.name
  | SymBind -> "~" ^ symbol.name

let string_of_pstring_node (node: pstring_node) =
  let symbol_string = string_of_term_symbol node.symbol in
  match node.index with
  | -1 -> symbol_string
  | _ -> Printf.sprintf "%d.%s" node.index symbol_string

let string_of_pstring (pstr: t) = String.concat "." (array_map_to_list string_of_pstring_node pstr)

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

(* (* Module for Set implementation *)
module IdComparableTerm = struct
  type t = term
  let compare a b = compare (2 * Obj.magic a) (2 * Obj.magic b)
end

(* Set of terms on a leaf of the index *)
module IndexLeafTermSet = Set.Make(IdComparableTerm) *)

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
    let indent = string_repeat "  " depth in
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
