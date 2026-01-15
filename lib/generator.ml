module IntMap = Map.Make (struct
  type t = int
  let compare = compare
end)

module StringMap = Map.Make (struct
  type t = string
  let compare = String.compare
end)

type env = {
  metas : Term.t IntMap.t;
  fvars : Term.t IntMap.t;
  scope : Term.t list;
  named : Term.t StringMap.t;
}

type ctx = {
  inputs : Term.t list;
  env : env;
  make_fvar : unit -> Term.t; (* callback that creates fresh variables *)
  make_symbol : arity:int -> Term.t list -> Term.t;
}

type gval = V_string of string

module type GEN = sig
  val name : string
  val run : ctx -> gval list -> Term.t option
end

let registry : (string, ctx -> gval list -> Term.t option) Hashtbl.t =
  Hashtbl.create 16

let register (module G : GEN) = Hashtbl.replace registry G.name G.run
let find name = Hashtbl.find_opt registry name

let eval name ctx args =
  match find name with
  | Some f -> Some (f ctx args)
  | None ->
      Log.error "unknown_generator:%s" name;
      None

module Fresh = struct
  let name = "fresh"
  let run ctx _ = Some (ctx.make_fvar ())
end

module Cte = struct
  let name = "cte_a"
  let run ctx _ = Some (ctx.make_symbol ~arity:0 [])
end

module Skolem = struct
  let name = "skolem"

  let run ctx _ =
    let scope = ctx.env.scope in
    let arity = List.length scope in
    Some (ctx.make_symbol ~arity scope)
end
