(* Tests that the linear feedback shift register algorithm produces
a uniform dirstribution when applying modulo n on the output *)

(* pgeon/lib $
ocamlc -c utils.mli
*)
(* pgeon/lib $
ocamlc -o lfsr_pseudo_random.exe utils.ml lfsr_random.ml demo/lfsr_pseudo_random.ml
*)

open Pgeon

module MultiSet = Map.Make(Int)

let _ =
  let range_size = 32 in
  let generation_count = range_size * 4096 in
  let seed = Lfsr_random.choose_seed () in (* 1 *)
  let bar_length = 40 in

  let table = MultiSet.empty in
  let add_record table number =
    let update_f search = match search with
    | None -> Some 1
    | Some count -> Some (count + 1)
    in
    MultiSet.update number update_f table
  in
  let range = List.init generation_count (fun x -> x + 1) in
  let compund_number (acc: int MultiSet.t * Lfsr_random.t) (_arg: int) =
    let (table, number) = acc in
    let result = Lfsr_random.next number in
    let kept = Lfsr_random.get_current result range_size in
    (add_record table kept, result)
  in
  let (end_table, _final) = List.fold_left compund_number (table, seed) range in
  let kvps = MultiSet.to_seq end_table in
  let string_of kvp =
    let (number, count) = kvp in
    let bars = Utils.string_repeat "|" (count * bar_length * range_size / generation_count) in
    (Printf.sprintf "%2d: %4d  %s" number count bars)
  in
  let kvp_strings = List.of_seq (Seq.map string_of kvps) in
  let distribution = String.concat "\n" kvp_strings in
  Printf.printf "distribution:\n%s\n" distribution;
  ;;
