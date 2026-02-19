type t = {
  next_branch_id : int;
  branches : (int * int list) list;
  formulas : Term.t list;
}

let init branch =
  if branch = [] then
    let _ = Log.error "[tree:init] status=error reason=empty_branch\n" in
    failwith "empty_input_branch"
  else
    {
      next_branch_id = 1;
      branches = [ (0, List.init (List.length branch) Fun.id) ];
      formulas = branch;
    }

let has_open_branches tree = List.length tree.branches > 0

let get_open_branches tree =
  Seq.unfold
    (fun branches ->
      match branches with
      | [] -> None
      | (id, indices) :: rest ->
          let terms = List.map (List.nth tree.formulas) indices in
          Some ((id, terms), rest))
    tree.branches

let close tree p =
  let matched, remaining =
    List.partition
      (fun (_id, indices) ->
        let terms = List.map (List.nth tree.formulas) indices in
        p terms)
      tree.branches
  in
  let new_tree = { tree with branches = remaining } in
  (new_tree, List.length matched > 0)

(* remove the branch with the given id *)
let remove tree branch_id =
  let remaining =
    List.filter (fun (id, _indices) -> id <> branch_id) tree.branches
  in
  { tree with branches = remaining }

(* val add_branch : t -> Term.t list -> t *)
let add_branch tree branch =
  if branch = [] then
    let _ = Log.error "[tree:add_branch] status=error reason=empty_branch\n" in
    failwith "empty_input_branch"
  else
    let branch_id = tree.next_branch_id in
    let indices = List.mapi (fun i _ -> i + List.length tree.formulas) branch in
    let formulas = tree.formulas @ branch in
    {
      next_branch_id = branch_id + 1;
      branches = (branch_id, indices) :: tree.branches;
      formulas;
    }

let map_formulas f tree = { tree with formulas = List.map f tree.formulas }
