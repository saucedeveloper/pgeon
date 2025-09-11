type t = { node : int; childs : t list option }

let init lst =
  let rec init id = function
    | [] ->
        Log.error "[tree:init] status=error reason=empty_formula_list\n";
        exit 1
    | [ _ ] -> { node = id; childs = Some [] }
    | _ :: tl -> { node = id; childs = Some [ init (id + 1) tl ] }
  in
  init 0 lst

let close t branch_index =
  let rec aux t =
    match t.childs with
    | Some childs ->
        if t.node = branch_index then { t with childs = None }
        else { t with childs = Some (List.map aux childs) }
    | None -> t
  in
  aux t

let find_leftmost_branch (tree : t) =
  let rec dfs acc node =
    match node.childs with
    | None -> None
    | Some [] -> Some (List.rev (node.node :: acc), node.node)
    | Some childs ->
        let rec search = function
          | [] -> None
          | child :: rest -> (
              match dfs (node.node :: acc) child with
              | None -> search rest
              | some -> some)
        in
        search childs
  in
  dfs [] tree
