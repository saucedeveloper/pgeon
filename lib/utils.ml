(* list all permutations length n of a list *)
let perm n l = 
  let rec aux k available =
    if k = 0 then
      [ [] ]
    else
      List.concat_map
        (fun x ->
          let remaining = List.filter (( != ) x) available in
          List.map (fun suffix -> x :: suffix) (aux (k - 1) remaining))
        available
  in
  if n < 0 then
    invalid_arg "perm_n: negative size"
  else if n > List.length l then
    []
  else
    aux n l

