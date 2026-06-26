(* Experiment to test whether objects are copied
in recursive types. Spoiler: they are not. *)

(* pgeon/lib $
ocamlc -o recursive_experiment.exe demo/recursive_experiment.ml
*)

type llist =
| Nil
| Next of string * llist

let _ =
  let run = true in
  if not run then () else
  let persons = Next ("joe", Nil) in
  let persons_b1 = Next ("jack", persons) in
  let rec string_id list =
    match list with
    | Nil -> Printf.sprintf "nil(@%d)" (2*Obj.magic list)
    | Next (name, next) -> Printf.sprintf "'%s'(@%d)->%s" name (2*Obj.magic list) (string_id next)
  in
  Printf.printf "persons: %s\n" (string_id persons);
  Printf.printf "persons_b1: %s\n" (string_id persons_b1);
  ;;
