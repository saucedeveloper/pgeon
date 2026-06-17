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
type pstring = t

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

module PstringSetElement = struct
  type t = pstring
  let compare = compare
end

module PstringSet = Set.Make(PstringSetElement)

let _ =
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
  let all_pstrings = Dynarray.create () in
  let print_term_pstrings term =
    Printf.printf "term: %s\n" (Factory.string_of_term term);
    let pstrings = make_pstrings term in
    Dynarray.append_list all_pstrings pstrings;
    Printf.printf "pstrings: { %s }\n" (
      String.concat ", " (List.map string_of_pstring pstrings)
    );
    ()
  in
  List.iter print_term_pstrings terms;
  let pstring_set = PstringSet.of_seq (Dynarray.to_seq all_pstrings) in
  let string_of_pstring_set pstring_set =
    let pstring_list = PstringSet.to_list pstring_set in
    let strings = List.map string_of_pstring pstring_list in
    Printf.sprintf "{ %s }" (String.concat ", " strings)
  in
  Printf.printf "All pstrings: %s\n" (string_of_pstring_set pstring_set);
  ;;
