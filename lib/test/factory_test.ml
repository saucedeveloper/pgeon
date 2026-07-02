open Pgeon

(* factory_test *)
let%test _ =
  let factory1 = Term.empty_factory in
  let (e, factory2) = Term.create_mvar "e" factory1 in
  let (p, factory3) = Term.create_mvar "p" factory2 in
  let (_, factory4) = Term.create_mvar "p" factory3 in
  let (bind_ex, factory5) = Term.create_bind "exists" p factory4 in
  let (bind_fa, factory6) = Term.create_bind "forall" e factory5 in
  let (bind_fa2, factory7) = Term.create_bind "forall" e factory6 in
  let (bind_fa3, factory8) = Term.create_bind "forall" e factory7 in
  let (app_and1, factory9) = Term.create_app "and" [e; p; bind_ex] factory8 in
  let (app_and2, factory10) = Term.create_app "and" [e; p; bind_fa] factory9 in
  let (app_and3, factory11) = Term.create_app "and" [e; p; bind_fa] factory10 in
  (bind_fa = bind_fa2) &&
  (bind_fa = bind_fa3) &&
  (bind_ex <> bind_fa) &&
  (app_and1 <> app_and2) &&
  (app_and2 = app_and3) &&
  (Term.term_equal bind_fa bind_fa2) &&
  (Term.term_equal bind_fa bind_fa3) &&
  not (Term.term_equal bind_ex bind_fa) &&
  not (Term.term_equal app_and1 app_and2) &&
  (Term.term_equal app_and2 app_and3) &&
  ((Term.factory_cardinal factory11) = 6)

(* Checking access to members of Factory *)
let%test _ =
  (* let _directly_constructed = Factory.Bvar 14 in *)
  (* Compiler error: Unbound constructor Bvar *)

  (* let variant_index term = (
    match term with
    | Bvar index -> 0
    | Fvar name -> 1
    | Mvar name -> 2
    | App (name, terms) -> 3
    | Bind (name, _term) -> 4
  ) in *)
  (* Compiler error: Unbound constructor Bvar (because node ref) *)

  (* let variant_index (term: Term.t) = (
    match !term with
    | Bvar index -> 0
    | Fvar name -> 1
    | Mvar name -> 2
    | App (name, terms) -> 3
    | Bind (name, _term) -> 4
  ) in *)
  (* Compiler error: The value term has type Term.t
    but an expression was expected of type 'a ref *)

  let term_to_int term = (
    match term with
    | Term.Bvar index -> 1 + index
    | Term.Fvar name -> 2 + (String.length name)
    | Term.Mvar name -> 3 + (String.length name)
    | Term.App (name, terms) -> 4 + (String.length name) + (List.length terms)
    | Term.Bind (name, _term) -> 5 + (String.length name)
  ) in
  let factory = Term.empty_factory in
  let (e, _) = Term.create_mvar "e" factory in
  (term_to_int e) = 4
