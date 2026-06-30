(* Type analogue to Term.t designed for easier (but sub-optimal) creation *)
type template =
| TBvar of int
| TFvar of Term.name
| TMvar of Term.name
| TApp  of Term.name * template list
| TBind of Term.name * template

let rec create_from_template (template: template) (factory: Term.factory) =
  match template with
  | TBvar i -> Term.create_bvar i factory
  | TFvar name -> Term.create_fvar name factory
  | TMvar name -> Term.create_mvar name factory
  | TApp (name, args) -> (
    let create (fac: Term.factory) (inner: template) =
      let (created, next_factory) = create_from_template inner fac in
      (next_factory, created)
    in
    let (next_factory, args_created) = List.fold_left_map create factory args in
    Term.create_app name args_created next_factory
  )
  | TBind (name, inner) -> (
    let (arg_created, next_factory) = create_from_template inner factory in
    Term.create_bind name arg_created next_factory
  )

let create_many (templates: template list) (factory: Term.factory) =
  let f fac temp =
    let (created, next_factory) = create_from_template temp fac in
    (next_factory, created)
  in
  let (next_factory, terms) = List.fold_left_map f factory templates in
  (terms, next_factory)
