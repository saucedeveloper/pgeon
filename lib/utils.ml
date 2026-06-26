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

let rec string_repeat str count =
  match count with
  | 1 -> str
  | _ when 1 < count -> str ^ string_repeat str (count - 1)
  | _ -> ""

let aggregate_self (transform: 'a -> 'a) (initial: 'a) (count: int) =
  let rec recursive (iter_count: int) (accumulater: 'a) =
    assert (iter_count <= count);
    if iter_count = count then
      accumulater
    else
      recursive (iter_count + 1) (transform accumulater)
  in
  recursive 0 initial

let aggregate_self_i (transform: int -> 'a -> 'a) (initial: 'a) (count: int) =
  let rec recursive (iter_count: int) (accumulater: 'a) =
    assert (iter_count <= count);
    if iter_count = count then
      accumulater
    else
      recursive (iter_count + 1) (transform iter_count accumulater)
  in
  recursive 0 initial

let aggregate_self_until (transform: int -> 'a -> ('a option)) (initial: 'a) (count: int) =
  let rec recursive (iter_count: int) (accumulater: 'a) =
    assert (iter_count <= count);
    if iter_count = count then
      accumulater
    else (
      let new_accumulater = transform iter_count accumulater in
      match new_accumulater with
      | None -> accumulater
      | Some new_acc -> recursive (iter_count + 1) new_acc
    )
  in
  recursive 0 initial

(* Address / unique id for x for printing *)
let address_of x = 2 * (Obj.magic x) (* / 2 *)

let string_address_of ?(n=4) x =
  let s = string_of_int (address_of x) in
  let len_s = String.length s in
  let start = len_s - n in
  if start <= 0 then
    s
  else
    String.sub s start n

let string_of_bool b = if b then "true" else "false"
