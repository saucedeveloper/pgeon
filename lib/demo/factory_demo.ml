(* pgeon/lib $ ocamlc factory.ml demo/factory_demo.ml -o factory_demo.exe *)

let _ =
  let run = true in
  if not run then () else
  (* let a_or_b = App("or", [ref (Fvar "a"); ref (Fvar "b")]) in
  for i = 0 to (Array.length Sys.argv) - 1 do
    Printf.printf "argv[%d] = %s\n" (i) Sys.argv.(i);
  done;
  Printf.printf "a_or_b: %s" (string_of_term a_or_b);; *)
  (*
  let arg_a = ref (Bind ("exists", p)) in
  let arg_b = ref (Bind ("forall", e)) in
  let comparison = term_compare arg_a arg_b in
  Printf.printf "arg_a: %s\n" (string_of_term !arg_a);
  Printf.printf "arg_b: %s\n" (string_of_term !arg_b);
  Printf.printf "comparison: %s\n" (string_of_int comparison);
  *)

  let factory1 = empty in
  let (p, factory2) = Factory.create_mvar "p" factory1 in
  let (e, factory3) = Factory.create_mvar "e" factory2 in
  let (_, factory4) = Factory.create_mvar "p" factory3 in
  let (arg_b, factory5) = Factory.create_bind "forall" e factory4 in
  let (arg_a, factory6) = Factory.create_bind "exists" p factory5 in
  Printf.printf "arg_b: %s\n" (Factory.string_address_of arg_b);

  let (arg_b2, factory7) = Factory.create_bind "forall" e factory6 in
  Printf.printf "arg_b2: %s\n" (Factory.string_address_of arg_b2);
  Printf.printf "arg_b: %s\n" (Factory.string_address_of arg_b);

  let _ = Factory.create_bvar 69420 factory1 in
  let _ = Factory.create_fvar "69420" factory1 in

  let (_, factory8) = Factory.create_app "f" [arg_a; arg_b2; arg_b] factory7 in

  Printf.printf "factory1 (full): %s\n"   (Factory.string_of_factory factory1);
  Printf.printf "factory1 (debg): %s\n\n" (Factory.debug_string_of_factory factory1);
  Printf.printf "factory2 (full): %s\n"   (Factory.string_of_factory factory2);
  Printf.printf "factory2 (debg): %s\n\n" (Factory.debug_string_of_factory factory2);
  Printf.printf "factory3 (full): %s\n"   (Factory.string_of_factory factory3);
  Printf.printf "factory3 (debg): %s\n\n" (Factory.debug_string_of_factory factory3);
  Printf.printf "factory4 (full): %s\n"   (Factory.string_of_factory factory4);
  Printf.printf "factory4 (debg): %s\n\n" (Factory.debug_string_of_factory factory4);
  Printf.printf "factory5 (full): %s\n"   (Factory.string_of_factory factory5);
  Printf.printf "factory5 (debg): %s\n\n" (Factory.debug_string_of_factory factory5);
  Printf.printf "factory6 (full): %s\n"   (Factory.string_of_factory factory6);
  Printf.printf "factory6 (debg): %s\n\n" (Factory.debug_string_of_factory factory6);
  Printf.printf "factory7 (full): %s\n"   (Factory.string_of_factory factory7);
  Printf.printf "factory7 (debg): %s\n\n" (Factory.debug_string_of_factory factory7);
  Printf.printf "factory8 (full): %s\n"   (Factory.string_of_factory factory8);
  Printf.printf "factory8 (debg): %s\n\n" (Factory.debug_string_of_factory factory8);
  ;;
