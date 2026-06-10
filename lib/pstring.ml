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
  let rec recursive (term: Factory.term) (current_path: t) (current_index: int) =
    let created_node = { index = current_index; symbol = get_term_symbol term } in
    let resulting_path = Array.append current_path [|created_node|] in
    match term with
    | Bvar _ | Fvar _ | Mvar _ -> [resulting_path]
    | App (name, terms) -> (
      let f inner index = recursive inner resulting_path index in
      let created_paths_by_term = list_map_index f terms in
      let inner_created = List.concat created_paths_by_term in
      inner_created
    )
    | Bind (name, inner) -> (
      let inner_created = recursive inner resulting_path 0 in
      inner_created
    )
  in
  recursive term [||] pstring_node_root_index

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

let string_of_pstring_in term pstring =
  let rec recursive (pstring_rem: t)(current_term: term) =
    match pstring_rem with
    | [||] -> ""
    | [|last_index|] -> (
      let subterm = get_subterm current_term last_index in
      match subterm with
      | Some subterm -> (
        let id_of_subterm = Factory.identifier_of_term subterm in
        let result = "." ^ (string_of_int last_index) ^ "." ^ id_of_subterm in
        (* Printf.printf "[last_index %d].result: %s\n" depth result; *)
        result
      )
      | None -> assert(false);
    )
    | _ ->
      let index = Array.get pstring_rem 0 in
      let new_pstring_rem = Array.slice pstring_rem 1 ((Array.length pstring_rem) - 1) in
      let subterm = get_subterm current_term index in
      match subterm with
      | None -> assert(false);
      | Some subterm -> (
        let id_of_subterm = Factory.identifier_of_term subterm in
        let result = "." ^ (string_of_int index) ^ "." ^ id_of_subterm ^ (recursive new_pstring_rem subterm) in
        (* Printf.printf "[inbetween %d].result: %s\n" depth result; *)
        result
      )
  in
  (Factory.identifier_of_term term) ^ recursive pstring term

let string_of_pstring pstr = String.concat "." (List.map string_of_int pstr)

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
  Printf.printf "pstrings in term: { %s }\n" (
    String.concat ", " (List.map (string_of_pstring_in a_f) pstrings)
  );
  ;;
