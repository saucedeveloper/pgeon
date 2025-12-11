type ctx = {
  inputs : Term.t list;
  make_fvar : unit -> Term.t; (* callback that creates fresh variables *)
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
