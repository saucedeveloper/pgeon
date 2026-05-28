(* list all permutations length n of a list *)
let perm n l =
  let rec aux k available =
    if k = 0 then [ [] ]
    else
      List.concat_map
        (fun x ->
          let remaining = List.filter (( != ) x) available in
          List.map (fun suffix -> x :: suffix) (aux (k - 1) remaining))
        available
  in
  if n < 0 then invalid_arg "perm_n: negative size"
  else if n > List.length l then []
  else aux n l

let diagonal rows =
  let rec next outer active buffer () =
    match buffer with
    | x :: rest -> Seq.Cons (x, next outer active rest)
    | [] -> (
        (* add one new row per diagonal round. *)
        let outer_done, outer', active' =
          match outer () with
          | Seq.Nil -> (true, Seq.empty, active)
          | Seq.Cons (row, outer_tail) -> (false, outer_tail, row :: active)
        in
        (* take one element from each active row. *)
        let rec step_rows rows acc_values acc_active =
          match rows with
          | [] -> (List.rev acc_values, List.rev acc_active)
          | row :: rest -> (
              match row () with
              | Seq.Nil -> step_rows rest acc_values acc_active
              | Cons (x, row_tail) ->
                  step_rows rest (x :: acc_values) (row_tail :: acc_active))
        in
        let values, active'' = step_rows active' [] [] in

        match (values, active'', outer_done) with
        | [], [], true -> Seq.Nil
        | [], _, _ -> next outer' active'' [] ()
        | _ -> next outer' active'' values ())
  in
  next rows [] []
