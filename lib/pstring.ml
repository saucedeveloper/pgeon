open Factory

(*
Path string: list of indices decribing the traversal of a term
| Bvar | Fvar | Mvar -> leaf
| App -> index in {0..arity}
| Bind -> 0 (* Because there is only one term *)
*)
type t = int list

let list_map_index (f: 'a -> int -> 'b) (list: 'a list) =
  let rec recursive remainder index = match remainder with
    | [] -> []
    | hd::tl -> (f hd index)::(recursive tl (index + 1))
  in
  recursive list 0

(* Make all the pstrings / root-to-leaf traversals in `term` *)
let make_pstrings term =
  (* Returns the list of created paths *)
  let rec recursive (term: Factory.term) (current_path: t) (paths: t list) = (
    match term with
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
    )
  ) in
  recursive term [] []

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
    | [] -> ""
    | [last_index] -> (
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
    | index::new_pstring_rem ->
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
