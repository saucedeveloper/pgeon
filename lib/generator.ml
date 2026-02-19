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
  | Some f -> f ctx args
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

module Inst = struct
  module TermSet = Set.Make (struct
    type t = Term.t

    let compare = compare
  end)

  let name = "inst"

  let rec is_closed = function
    | Term.Bvar _ -> false
    | Term.Fvar _ -> false
    | Term.Mvar _ -> false
    | Term.App (_f, args) -> List.for_all is_closed args
    | Term.Bind (_b, body) -> is_closed body

  let rec term_to_string = function
    | Term.Bvar i -> Printf.sprintf "B%d" i
    | Term.Fvar i -> Printf.sprintf "F%d" i
    | Term.Mvar i -> Printf.sprintf "M%d" i
    | Term.App (f, args) ->
        let args =
          match args with
          | [] -> ""
          | _ ->
              args |> List.map term_to_string |> String.concat ", "
              |> Printf.sprintf "(%s)"
        in
        Printf.sprintf "f%d%s" f args
    | Term.Bind (b, body) ->
        Printf.sprintf "bind%d(%s)" b (term_to_string body)

  let key_of_terms terms =
    terms |> Array.to_list |> List.map term_to_string |> String.concat "|"

  let rec collect_term acc term =
    let acc =
      match term with
      | Term.App _ when is_closed term -> TermSet.add term acc
      | _ -> acc
    in
    match term with
    | Term.Bvar _ | Term.Fvar _ | Term.Mvar _ -> acc
    | Term.App (_f, args) ->
        List.fold_left collect_term acc args
    | Term.Bind (_, body) -> collect_term acc body

  let collect_from_inputs ctx =
    List.fold_left collect_term TermSet.empty ctx.inputs

  let collect_from_env ctx acc =
    IntMap.fold (fun _ term acc -> collect_term acc term) ctx.env.metas acc

  let cache : (string, (Term.t array * int ref)) Hashtbl.t = Hashtbl.create 16

  let run ctx _ =
    let terms_set = ctx |> collect_from_inputs |> collect_from_env ctx in
    let terms = TermSet.elements terms_set in
    match terms with
    | [] -> None
    | _ ->
        let arr = Array.of_list terms in
        let key = key_of_terms arr in
        let entry =
          match Hashtbl.find_opt cache key with
          | Some (cached_arr, idx) when Array.length cached_arr = Array.length arr
            ->
              (cached_arr, idx)
          | _ ->
              let idx = ref 0 in
              Hashtbl.replace cache key (arr, idx);
              (arr, idx)
        in
        let arr, idx = entry in
        if Array.length arr = 0 then None
        else (
          let term = arr.(!idx) in
          idx := (!idx + 1) mod Array.length arr;
          Some term)
end
