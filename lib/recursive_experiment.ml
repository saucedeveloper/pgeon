type llist =
| Nil
| Next of string * llist

let _ () =
  let rec string_id list =
    match list with
    | Nil -> Printf.sprintf "nil(@%d)" (2*Obj.magic list)
    | Next (name, next) -> Printf.sprintf "'%s'(@%d)->%s" name (2*Obj.magic list) (string_id next)
