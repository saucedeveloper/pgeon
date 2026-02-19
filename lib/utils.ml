let option_all l =
  List.fold_right
    (fun o acc ->
      match (o, acc) with Some v, Some vs -> Some (v :: vs) | _, _ -> None)
    l (Some [])

let option_all2 l =
  List.fold_right
    (fun l acc ->
      match (option_all l, acc) with
      | Some v, Some vs -> Some (v :: vs)
      | _, _ -> None)
    l (Some [])

let seq_to_list seq = Seq.fold_left (fun acc x -> x :: acc) [] seq |> List.rev
