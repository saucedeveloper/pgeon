(* Type analogue to Term.t designed for easier (but sub-optimal) creation *)
type template =
| TBvar of int
| TFvar of Term.name
| TMvar of Term.name
| TApp  of Term.name * template list
| TBind of Term.name * template

type term_id =
| IBvar of int
| IFvar of Term.name
| IMvar of Term.name
| IApp  of Term.name
| IBind of Term.name

let tbvar (i: int) = TBvar i

let tfvar (name: Term.name) = TFvar name

let tmvar (name: Term.name) = TMvar name

let tapp (name: Term.name) (args: template list) = TApp (name, args)

let tconst (name: Term.name) = TApp (name, [])

let tbind (name: Term.name) (arg: template) = TBind (name, arg)

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

let create_many (templates: template Seq.t) (factory: Term.factory) =
  let create fac temp =
    let (created, next_factory) = create_from_template temp fac in
    (next_factory, created)
  in
  let (next_factory, terms) = Utils.seq_fold_left_map_to_list create factory templates in
  (terms, next_factory)

let string_of_full ?(n=4) (term: Term.t) =
  Printf.sprintf "%s{@%s}" (Term.string_of term) (Utils.string_address_of ~n:n term)

let string_of_term_id = function
| IBvar i -> "Bvar #" ^ (string_of_int i)
| IFvar s -> "Fvar '" ^ s
| IMvar s -> "Mvar ?" ^ s
| IApp s -> "App " ^ s
| IBind s -> "Bind " ^ s

(* let string_append_char str chr =
  String.of_seq (Seq.append (String.to_seq str) (Seq.singleton chr)) *)

let string_sub_to_end str i =
  if i < 0 || (String.length str) <= i then
    raise (Invalid_argument "index out of bounds")
  else
    String.sub str i ((String.length str) - i)

let extract_identifier text =
  let identifier_length is_legal_char prefix_length =
    let text_as_seq = String.to_seq text in
    let char_seq = Seq.drop prefix_length text_as_seq in
    let charsi = Seq.zip char_seq (Seq.ints 0) in
    let is_illegal_char kvp =
      let (chr, i) = kvp in
      not (is_legal_char chr i)
    in
    Seq.find_index is_illegal_char charsi
  in
  let text_length = String.length text in
  if 0 < text_length then (
    match (String.get text 0) with
    | '#' -> (
      if text_length <= 1 then
        None
      else (
        let is_legal char _i = Char.Ascii.is_digit char in
        let id_length = Option.value (identifier_length is_legal 1) ~default:(text_length - 1) in
        let number_as_str = String.sub text 1 id_length in
        Option.map (fun number -> (IBvar number, id_length)) (int_of_string_opt number_as_str)
      )
    )
    | '\'' -> (
      if text_length <= 1 then
        None
      else (
        let is_legal char i = (Char.Ascii.is_letter char)
          || ((Char.Ascii.is_digit char) && (1 <= i)) in
        let id_length = Option.value (identifier_length is_legal 1) ~default:(text_length - 1) in
        let name = String.sub text 1 id_length in
        Some (IFvar name, id_length)
      )
    )
    | '?' -> (
      if text_length <= 1 then
        None
      else (
        let is_legal char i = (Char.Ascii.is_letter char)
          || ((Char.Ascii.is_digit char) && (1 <= i)) in
        let id_length = Option.value (identifier_length is_legal 1) ~default:(text_length - 1) in
        let name = String.sub text 1 id_length in
        Some (IMvar name, id_length)
      )
    )
    | _ -> (
      let is_legal char i = (Char.Ascii.is_letter char)
        || ((Char.Ascii.is_digit char) && (1 <= i)) in
      let id_length = Option.value (identifier_length is_legal 0) ~default:text_length in
      (* Printf.printf "id_length: %d\n" id_length; *)
      assert (id_length <= text_length);
      let () = if (id_length = 0) then (
        Printf.printf "text: %s\n" text;
      ) else () in
      assert (0 < id_length);
      if text_length = id_length then
        Some (IApp text, id_length)
      else (
        assert (id_length < text_length);
        let name = String.sub text 0 id_length in
        let suffix = String.get text id_length in
        if suffix = '.' then
          Some (IBind name, id_length + 1)
        else
          Some (IApp name, id_length)
      )
    )
  )
  else
    None

type parse_break =
| NoBreak
| Comma
| OpenParen
| CloseParen

let string_of_break = function
| NoBreak -> "NoBreak"
| Comma -> "Comma"
| OpenParen -> "OpenParen"
| CloseParen -> "CloseParen"

let parse (text: string) (factory: Term.factory): (Term.t * Term.factory) option =

  let create_from_identifier identifier factory =
    match identifier with
    | IBvar i -> Some (Term.create_bvar i factory)
    | IFvar s -> Some (Term.create_fvar s factory)
    | IMvar s -> Some (Term.create_mvar s factory)
    | IApp s -> Some (Term.create_app s [] factory)
    | _ -> None
  in

  let rec rec_term remaining factory =
    let identifier_maybe = extract_identifier remaining in
    Printf.printf "identifying \"%s\" -> %s\n"
      remaining
      (match identifier_maybe with
      | None -> "None"
      | Some (id, consumed) -> (string_of_term_id id) ^ " (" ^ (string_of_int consumed) ^ ")");
    match identifier_maybe with
    | None -> ([], factory)
    | Some (identifier, id_consumed) -> (
      let id_remaining = string_sub_to_end remaining id_consumed in
      let break_result = rec_break id_remaining in
      match break_result with
      | Some (break, break_consumed) -> (
        let break_remaining = string_sub_to_end id_remaining break_consumed in
        Printf.printf "remaining: \"%s\", id_remaining: \"%s\", break: %s, break_remaining: \"%s\"\n"
          remaining id_remaining (string_of_break break) break_remaining;
        assert ((String.length break_remaining) < (String.length remaining));
        match break with
        | Comma -> (
          match create_from_identifier identifier factory with
          | Some (created_identifier, new_factory) -> (
            let (inner, inner_factory) = rec_term break_remaining new_factory in
            (created_identifier::inner, inner_factory)
          )
          | None -> assert (false);
        )
        | OpenParen -> (
          let (arguments, new_factory) = rec_term break_remaining factory in
          match identifier with
          | IApp name -> (
            let (created_app, new_factory) = Term.create_app name arguments new_factory in
            ([created_app], new_factory)
          )
          | IBind name -> (
            match arguments with
            | single::[] -> (
              let (created_bind, new_factory) = Term.create_bind name single new_factory in
              ([created_bind], new_factory)
            )
            | _ -> ([], new_factory)
          )
          | _ -> ([], new_factory)
        )
        | CloseParen | NoBreak -> (
          match create_from_identifier identifier factory with
          | Some (created_identifier, new_factory) -> (
            ([created_identifier], new_factory)
          )
          | None -> assert (false);
        )
      )
      | None -> ([], factory)
    )
  and rec_break remaining =
    let not_space chr = chr != ' ' in
    let first_break = String.find_first_index not_space remaining in
    match first_break with
    | None -> Some (NoBreak, String.length remaining)
    | Some break -> (
      let char = String.get remaining break in
      let consumed = break + 1 in
      match char with
      | '(' -> Some (OpenParen, consumed)
      | ')' -> Some (CloseParen, consumed)
      | ',' -> Some (Comma, consumed)
      | _ -> None
    )
  in
  let (terms, final_factory) = rec_term text factory in
  match terms with
  | single::[] -> Some (single, final_factory)
  | _ -> None
