open Pgeon
open Lexing

let column pos = pos.pos_cnum - pos.pos_bol + 1

let read_file parse filename =
  let parse_with lexbuf =
    try Ok (parse lexbuf) with
    | Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        Error
          (Printf.sprintf "%s:%d:%d: parse error" pos.pos_fname pos.pos_lnum
             (column pos))
    | Failure msg ->
        let pos = lexbuf.lex_curr_p in
        Error
          (Printf.sprintf "%s:%d:%d: %s" pos.pos_fname pos.pos_lnum (column pos)
             msg)
    | ex -> Error (Printexc.to_string ex)
  in
  let ic = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let lexbuf = Lexing.from_channel ic in
      Lexing.set_filename lexbuf filename;
      parse_with lexbuf)

let parse_logic filename = read_file (Parser.logic_file Lexer.token) filename

let parse_problem filename =
  read_file (Parser.problem_file Lexer.token) filename

let usage_msg =
  "Usage: pgeon [--log-level debug|info|warn|error] <logic-file> <problem-file>"

let parse_level = function
  | "debug" -> Some Log.Debug
  | "info" -> Some Log.Info
  | "warn" -> Some Log.Warn
  | "error" -> Some Log.Error
  | _ -> None

let () =
  let rec parse_args level files = function
    | [] -> Ok (level, List.rev files)
    | "--log-level" :: lvl :: rest -> (
        match (level, parse_level lvl) with
        | Some _, _ -> Error "Multiple --log-level flags provided"
        | None, None ->
            Error
              (Printf.sprintf
                 "Unknown log level '%s'. Expected one of debug, info, warn, \
                  error."
                 lvl)
        | None, Some lvl' -> parse_args (Some lvl') files rest)
    | "--log-level" :: [] ->
        Error "Missing value after --log-level (expected debug|info|warn|error)"
    | arg :: rest -> parse_args level (arg :: files) rest
  in
  let argv = Array.to_list Sys.argv |> List.tl in
  match parse_args None [] argv with
  | Error msg ->
      prerr_endline msg;
      prerr_endline usage_msg;
      exit 1
  | Ok (level_opt, [ logic_file; problem_file ]) -> (
      Log.set_level (Option.value level_opt ~default:Log.Warn);
      let logic_ast =
        match parse_logic logic_file with
        | Ok ast -> ast
        | Error msg ->
            prerr_endline msg;
            exit 1
      in
      let problem_ast =
        match parse_problem problem_file with
        | Ok ast -> ast
        | Error msg ->
            prerr_endline msg;
            exit 1
      in
      try
        if Tableau.prove logic_ast problem_ast then (
          print_endline "Success";
          exit 0)
        else (
          print_endline "Failure";
          exit 0)
      with ex ->
        prerr_endline (Printexc.to_string ex);
        print_endline "Error";
        exit 1)
  | Ok (_, _) ->
      prerr_endline usage_msg;
      exit 1
