type lfsr_state = int

let choose_seed (x: unit) =
  Random.self_init ();
  Random.full_int (int_of_float (1e18))

let next n0 =
  let n1 = n0 lxor (n0 lsr 7) in
  let n2 = n1 lxor (n1 lsl 9) in
  let n3 = n2 lxor (n2 lsr 13) in
  n3

let get_current state bound_max =
  abs (state mod bound_max)
