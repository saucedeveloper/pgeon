type level = Debug | Info | Warn | Error

let int_of_level = function Debug -> 0 | Info -> 1 | Warn -> 2 | Error -> 3

let string_of_level = function
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"

let min_level = ref Info

type formatter = level -> string -> string
type sink = level -> string -> unit

let formatter : formatter ref =
  ref (fun lvl msg -> Printf.sprintf "[%s] %s" (string_of_level lvl) msg)

let sink =
  ref (fun l s ->
      if int_of_level l >= int_of_level !min_level then
        match l with
        | Debug -> Printf.printf "%s%!" (!formatter l s)
        | Info -> Printf.printf "%s%!" (!formatter l s)
        | Warn -> Printf.eprintf "%s%!" (!formatter l s)
        | Error -> Printf.eprintf "%s%!" (!formatter l s))

let set_level lvl = min_level := lvl
let set_sink s = sink := s
let set_formatter f = formatter := f

let log lvl =
  let f =
    if int_of_level lvl >= int_of_level !min_level then !sink lvl
    else fun _ -> ()
  in
  Printf.ksprintf f

let debug fmt = log Debug fmt
let info fmt = log Info fmt
let warn fmt = log Warn fmt
let error fmt = log Error fmt
let get_level () = !min_level
