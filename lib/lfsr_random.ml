type t = int

let choose_seed () =
  Random.self_init ();
  Random.full_int (int_of_float (1e18))

let next n0 =
  let n1 = n0 lxor (n0 lsr 7) in
  let n2 = n1 lxor (n1 lsl 9) in
  let n3 = n2 lxor (n2 lsr 13) in
  n3

let get_current state bound_max =
  assert (0 < bound_max);
  abs (state mod bound_max)

let get_current_raw state = state

let get_current_raw_positive state = abs state

let get_next state bound_max =
  (get_current state bound_max, next state)

let string_of_state = string_of_int

let state_of_int i = i
