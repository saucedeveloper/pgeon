type generator =
  Tableau.proof_state -> Term.t -> (Term.t * Tableau.proof_state) option

type unifier = Term.t -> Term.t -> Term.free_substitution option

type t = {
  generators : (string, generator) Hashtbl.t;
  unifiers : (string, unifier) Hashtbl.t;
}

let create () = { generators = Hashtbl.create 16; unifiers = Hashtbl.create 16 }
let register_generator reg name fn = Hashtbl.replace reg.generators name fn
let register_unifier reg name fn = Hashtbl.replace reg.unifiers name fn
let find_generator reg name = Hashtbl.find reg.generators name
let find_unifier reg name = Hashtbl.find reg.unifiers name
