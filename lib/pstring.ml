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

type index_node = {
  term_id: Factory.name;
  sub_nodes: index_node array
}

(* type term_id_variant =
| BvarId
| FvarId
| MvarId
| AppId
| BindId

(* Term identifier used in the index *)
type term_id = {
  variant: term_id_variant;
  id: Factory.name;
} *)

let _ =
  let factory0 = Factory.empty in
  let (f_x, factory1) = Factory.create_fvar "x" factory0 in
  let (f_y, factory2) = Factory.create_fvar "y" factory1 in
  let (m_z, factory3) = Factory.create_mvar "z" factory2 in
  let (a_p, factory4) = Factory.create_app "P" [m_z] factory3 in
  let (b_e, factory5) = Factory.create_bind "exists" a_p factory4 in
  let (a_f, _factory6) = Factory.create_app "f" [f_x; b_e; f_y; a_p] factory5 in
  Printf.printf "term: %s\n" (string_of_term a_f);
  let pstrings = make_pstrings a_f in
  Printf.printf "pstrings: { %s }\n" (
    String.concat ", " (List.map string_of_pstring pstrings)
  );
  ;;
