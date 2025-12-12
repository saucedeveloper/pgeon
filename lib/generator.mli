type ctx = {
  inputs : Term.t list;
  make_fvar : unit -> Term.t;
}

type gval = V_string of string

module type GEN = sig
  val name : string
  val run : ctx -> gval list -> Term.t option
end

val registry : (string, ctx -> gval list -> Term.t option) Hashtbl.t
val register : (module GEN) -> unit
val find : string -> (ctx -> gval list -> Term.t option) option
val eval : string -> ctx -> gval list -> Term.t option option

module Fresh : sig
  val name : string
  val run : ctx -> 'a -> Term.t option
end
