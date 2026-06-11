open Factory

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
  | Bvar index -> { variant = SymBvar; name = string_of_int index }
  | Fvar name -> { variant = SymFvar; name = name }
  | Mvar name -> { variant = SymMvar; name = name }
  | App (name, _) -> { variant = SymApp; name = name }
  | Bind (name, _) -> { variant = SymBind; name = name }

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

(* Make all the pstrings / root-to-leaf traversals in `term` *)
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

let make_pstrings term =
  let shared_path: pstring_node Dynarray.t = Dynarray.create () in
  let rec recursive (term: Factory.term) (current_index: int) =
    let created_node = { index = current_index; symbol = get_term_symbol term } in
    Dynarray.add_last shared_path created_node;
    match term with
    | Bvar _ | Fvar _ | Mvar _ -> (
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
  | Bvar _ | Fvar _ | Mvar _ -> None
  | App (name, terms) -> (
    List.nth_opt terms index
  )
  | Bind (name, term) -> (
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

(* Module for Set implementation *)
module IdComparableTerm = struct
  type t = term
  let compare a b = compare (2 * Obj.magic a) (2 * Obj.magic b)
end

(* Set of terms on a leaf of the index *)
module IndexLeafTermSet = Set.Make(IdComparableTerm)

let string_of_term_set (term_set: IndexLeafTermSet.t) =
  let term_list = IndexLeafTermSet.to_list term_set in
  let string_of_term term = Factory.string_address_of term in
  let strings = List.map string_of_term term_list in
  Printf.sprintf "{ %s }" (String.concat ", " strings)

(* Node that contains subnodes based on argument position *)
type index_array_node = {
  (* symbol: term_symbol; *)
  sub_map_nodes: index_map_node Dynarray.t
}

(* Subnode of map node: either a sub array or a leaf containing the set of matching terms *)
and index_map_subnode =
| SubArray of index_array_node
| SubLeaf of IndexLeafTermSet.t

(* Node that contains subnodes based on term symbol *)
and index_map_node = {
  subnodes: (term_symbol, index_map_subnode) Hashtbl.t;
}

type term_index = {
  root: index_map_node;
}

let rec string_repeat str count =
  match count with
  | 1 -> str
  | _ when 1 < count -> str ^ string_repeat str (count - 1)
  | _ -> ""

let string_of_index (term_index: term_index) =
  let rec rec_array (current: index_array_node) (depth: int) =
    let indent = string_repeat "  " depth in
    let indent_plus = indent ^ "  " in
    (* (string_of_term_symbol current.symbol)
    ^ *) (if (Dynarray.length current.sub_map_nodes) = 0 then
        ""
      else (
        "(\n" ^
        let f i map_node = Printf.sprintf "%s%d: %s" indent_plus i (rec_map map_node (depth + 1)) in
        let subnode_strings = Dynarray.to_list (Dynarray.mapi f current.sub_map_nodes) in
        let concatenated = String.concat ",\n" subnode_strings in
        concatenated ^ "\n" ^
        indent ^ ")"
      )
    )
  and rec_map (current: index_map_node) (depth: int) =
    let indent = string_repeat "  " depth in
    let indent_plus = indent ^ "  " in
      (if (Hashtbl.length current.subnodes) = 0 then
        ""
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
        let kvp_sequence: (term_symbol * index_map_subnode) Seq.t = Hashtbl.to_seq current.subnodes in
        let subnode_strings = List.of_seq (Seq.map f kvp_sequence) in
        let concatenated = String.concat ",\n" subnode_strings in
        concatenated ^ "\n" ^
        indent ^ "}"
      )
    )
  in
  rec_map term_index.root 0

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
  (* Printf.printf "term: %s\n" (string_of_term a_f);
  let pstrings = make_pstrings a_f in
  Printf.printf "pstrings: { %s }\n" (
    String.concat ", " (List.map string_of_pstring pstrings)
  ); *)
  let index = {
    root = {
      subnodes = make_hashtbl [
        (get_term_symbol f1,
          SubArray {
            sub_map_nodes = Dynarray.of_list [
              {
                subnodes = make_hashtbl [
                  (get_term_symbol x,
                    SubLeaf (IndexLeafTermSet.of_list [f5])
                  );
                  (get_term_symbol g1,
                    SubArray {
                      sub_map_nodes = Dynarray.of_list [
                        {
                          subnodes = make_hashtbl [
                            (get_term_symbol x,
                              SubLeaf (IndexLeafTermSet.of_list [f2; f4])
                            );
                            (get_term_symbol a,
                              SubLeaf (IndexLeafTermSet.of_list [f1; f3])
                            );
                          ];
                        };
                        {
                          subnodes = make_hashtbl [
                            (get_term_symbol b,
                              SubLeaf (IndexLeafTermSet.of_list [f2; f3])
                            );
                            (get_term_symbol c,
                              SubLeaf (IndexLeafTermSet.of_list [f4])
                            );
                            (get_term_symbol x,
                              SubLeaf (IndexLeafTermSet.of_list [f1])
                            );
                          ];
                        }
                      ]
                    }
                  );
                ];
              };
              {
                subnodes = make_hashtbl [
                  (get_term_symbol b,
                    SubLeaf (IndexLeafTermSet.of_list [f4])
                  );
                  (get_term_symbol c,
                    SubLeaf (IndexLeafTermSet.of_list [f1; f3])
                  );
                  (get_term_symbol x,
                    SubLeaf (IndexLeafTermSet.of_list [f2; f5])
                  );
                ]
              }
            ];
          }
        )
      ]
    }
  } in
  Printf.printf "index:\n%s\n" (string_of_index index);
  ;;
