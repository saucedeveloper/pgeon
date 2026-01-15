module IntMap : Map.S with type key = int
module StringMap : Map.S with type key = string

type env = {
  metas : Term.t IntMap.t;
  fvars : Term.t IntMap.t;
  scope : Term.t list;
  named : Term.t StringMap.t;
}

type ctx = {
  inputs : Term.t list;
  env : env;
  make_fvar : unit -> Term.t;
  make_symbol : arity:int -> Term.t list -> Term.t;
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

module Cte : sig
  val name : string
  val run : ctx -> 'a -> Term.t option
end

module Skolem : sig
  val name : string
  val run : ctx -> gval list -> Term.t option
end
