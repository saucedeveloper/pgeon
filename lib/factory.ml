type term = Term.t

(* Address / unique id for x for printing *)
let address_of x = 2 * (Obj.magic x) / 2

let string_address_of ?(n=4) x =
  let s = string_of_int (address_of x) in
  String.sub s (String.length s - n) n

let term_compare (a: term) (b: term) = compare a b

let term_equal a b = (a == b)

(* Module for Set implementation *)
module FactoryTerm = struct
  type t = term
  let compare = term_compare
end

module FactoryTermSet = Set.Make(FactoryTerm)

(* Factory is a set of terms using term_compare to tell them apart *)
type t = {
  set : FactoryTermSet.t;
}

(* Empty factory *)
let empty = {
  set = FactoryTermSet.empty
}

(* term to string implementation from compile.ml *)
let string_of_term term =
  let rec string_of_term = function
      | Term.Bvar i -> Printf.sprintf "#%d" i
      | Term.Fvar x -> "'" ^ x
      | Term.Mvar x -> "?" ^ x
      | Term.App (f, []) -> f
      | Term.App (f, args) ->
          Printf.sprintf "%s(%s)" f
            (String.concat ", " (List.map string_of_term args))
      | Term.Bind (b, body) -> Printf.sprintf "%s.(%s)" b (string_of_term body)
    in
    string_of_term term

let identifier_of_term term =
  match term with
  | Term.Bvar i -> Printf.sprintf "%d" i
  | Term.Fvar x -> x
  | Term.Mvar x -> x
  | Term.App (f, args) -> f
  | Term.Bind (b, body) -> b

let create_or_get_term term factory =
  let existing_search = FactoryTermSet.find_opt term factory.set in (
  match existing_search with
  | Some existing ->
    (existing, factory)
  | None ->
    let new_factory = { set = (FactoryTermSet.add term factory.set) } in
    (term, new_factory)
  )

(* Create or get Bvar in factory *)
let create_bvar index factory = create_or_get_term (Bvar index) factory

(* Create or get Fvar in factory *)
let create_fvar name factory = create_or_get_term (Fvar name) factory

(* Create or get Mvar in factory *)
let create_mvar name factory = create_or_get_term (Mvar name) factory

(* Create or get App in factory *)
let create_app name terms factory = create_or_get_term (App (name, terms)) factory

(* Create or get Bind in factory *)
let create_bind name term factory = create_or_get_term (Bind (name, term)) factory

let cardinal factory = FactoryTermSet.cardinal factory.set

(* Factory to string *)
let string_of_factory factory =
  if (cardinal factory) = 0 then
    "{}"
  else
    let term_list = FactoryTermSet.to_list factory.set in
    let term_string_list = List.map string_of_term term_list in
    "{ " ^ (String.concat ", " term_string_list) ^ " }"

(* String of a term with strings of
recursive elements given by `custom_string_of` *)
let string_of_term_custom term custom_string_of =
  let string_of term = match term with
      | Term.Bvar i -> Printf.sprintf "#%d" i
      | Term.Fvar x -> "'" ^ x
      | Term.Mvar x -> "?" ^ x
      | Term.App (f, []) -> f
      | Term.App (f, args) ->
          Printf.sprintf "%s(%s)" f
            (String.concat ", " (List.map custom_string_of args))
      | Term.Bind (b, body) -> Printf.sprintf "%s.(%s)" b (custom_string_of body)
    in
    string_of term

(* String of factory where each element of depth > 0
is written as its address in factory *)
let debug_string_of_factory factory =
  if (FactoryTermSet.cardinal factory.set) = 0 then
    "{}"
  else
    let rec recursive depth term =
      if depth = 0 then
        let compact_string =
          string_of_term_custom term (recursive (depth + 1))
        in
        compact_string ^ "(@" ^ (string_address_of term) ^ ")"
      else
        let string_of_term =
          if (FactoryTermSet.mem term factory.set) then
            "{@" ^ (string_address_of term) ^ "}"
          else
            "{@?" ^ (string_address_of term) ^ "}"
        in
          string_of_term
      in

    let term_list = FactoryTermSet.to_list factory.set in
    let term_string_list = List.map (recursive 0) term_list in
    "{ " ^ (String.concat ", " term_string_list) ^ " }"
