(* pgeon/lib $ ocamlc utils.ml demo/lfsr_pseudo_random.ml -o lfsr_pseudo_random.exe *)

module MultiSet = Map.Make(Int)

let lfsr_next n0 =
  let n1 = n0 lxor (n0 lsr 7) in
  let n2 = n1 lxor (n1 lsl 9) in
  let n3 = n2 lxor (n2 lsr 13) in
  n3

let lfsr_normalize n max =
  abs (n mod max)

let _ =
  let range_size = 32 in
  let generation_count = range_size * 4096 in
  let seed: int = Lfsr_random.choose_seed () in (* 1 *)
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
  let compund_number (acc: int MultiSet.t * int) (arg: int) =
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
