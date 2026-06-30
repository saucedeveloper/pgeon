(* Tests the reutilization of structurally identical terms *)

(* pgeon/lib $
ocamlc -c utils.mli term.mli
*)
(* pgeon/lib $
ocamlc -o factory_demo.exe utils.ml term.ml demo/factory_demo.ml
*)

open Pgeon
open Utils
open Term

let _ =
  let run = true in
  if not run then () else

  let factory1 = empty_factory in
  let (p, factory2) = create_mvar "p" factory1 in
  let (e, factory3) = create_mvar "e" factory2 in
  let (_, factory4) = create_mvar "p" factory3 in
  let (arg_b, factory5) = create_bind "forall" e factory4 in
  let (arg_a, factory6) = create_bind "exists" p factory5 in
  Printf.printf "arg_b: %s\n" (string_address_of arg_b);

  let (arg_b2, factory7) = create_bind "forall" e factory6 in
  Printf.printf "arg_b2: %s\n" (string_address_of arg_b2);
  Printf.printf "arg_b: %s\n" (string_address_of arg_b);

  let _ = create_bvar 69420 factory1 in
  let _ = create_fvar "69420" factory1 in

  let (_, factory8) = create_app "f" [arg_a; arg_b2; arg_b] factory7 in

  Printf.printf "factory1 (full): %s\n"   (string_of_factory factory1);
  Printf.printf "factory1 (debg): %s\n\n" (debug_string_of_factory factory1);
  Printf.printf "factory2 (full): %s\n"   (string_of_factory factory2);
  Printf.printf "factory2 (debg): %s\n\n" (debug_string_of_factory factory2);
  Printf.printf "factory3 (full): %s\n"   (string_of_factory factory3);
  Printf.printf "factory3 (debg): %s\n\n" (debug_string_of_factory factory3);
  Printf.printf "factory4 (full): %s\n"   (string_of_factory factory4);
  Printf.printf "factory4 (debg): %s\n\n" (debug_string_of_factory factory4);
  Printf.printf "factory5 (full): %s\n"   (string_of_factory factory5);
  Printf.printf "factory5 (debg): %s\n\n" (debug_string_of_factory factory5);
  Printf.printf "factory6 (full): %s\n"   (string_of_factory factory6);
  Printf.printf "factory6 (debg): %s\n\n" (debug_string_of_factory factory6);
  Printf.printf "factory7 (full): %s\n"   (string_of_factory factory7);
  Printf.printf "factory7 (debg): %s\n\n" (debug_string_of_factory factory7);
  Printf.printf "factory8 (full): %s\n"   (string_of_factory factory8);
  Printf.printf "factory8 (debg): %s\n\n" (debug_string_of_factory factory8);
  ;;
