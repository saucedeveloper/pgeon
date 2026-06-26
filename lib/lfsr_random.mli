(* Simple controllable pseudo random number generation *)

(* Pseudo random state *)
type t

(* Choose a state at random *)
val choose_seed : unit -> t

(* Next state in the pseudo random sequence *)
val next : t -> t

(* Current value for this state and this exclusive upper bound *)
val get_current : t -> int -> int

(* Current value for this state (could be any int) *)
val get_current_raw : t -> int

(* Current value for this state (could be any positive int) *)
val get_current_raw_positive : t -> int

(* ((Current value for this state), (the following state)) *)
val get_next : t -> int -> (int * t)

(* State to string *)
val string_of_state : t -> string

(* State from int *)
val state_of_int : int -> t
