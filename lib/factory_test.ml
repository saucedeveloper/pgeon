
(* factory_test *)
let%test _ =
  let factory1 = Factory.empty in
  let (e, factory2) = Factory.create_mvar "e" factory1 in
  let (p, factory3) = Factory.create_mvar "p" factory2 in
  let (_, factory4) = Factory.create_mvar "p" factory3 in
  let (arg_a, factory5) = Factory.create_bind "exists" p factory4 in
  let (arg_b, factory6) = Factory.create_bind "forall" e factory5 in

  let (arg_b2, factory7) = Factory.create_bind "forall" e factory6 in
  (arg_b = arg_b2) &&
  (arg_a <> arg_b) &&
  ((Factory.cardinal factory7) = 4) &&
  (Factory.term_equal arg_b arg_b2) &&
  not (Factory.term_equal arg_a arg_b)

(* comparison_bvar *)
let%test _ =
  let factory = Factory.empty in
  let (bvar0, factory) = Factory.create_bvar 0 factory in
  let (bvar1, factory) = Factory.create_bvar 1 factory in
  let (bvar5, factory) = Factory.create_bvar 5 factory in
  let (bvarm7, _factory) = Factory.create_bvar (-7) factory in
  ((Factory.term_compare bvar0 bvar1) < 0) &&
  ((Factory.term_compare bvar5 bvarm7) > 0) &&
  ((Factory.term_compare bvar5 bvar5) = 0)

(* comparison_fvar *)
let%test _ =
  let factory = Factory.empty in
  let (fvar_x2, factory) = Factory.create_fvar "X2" factory in
  let (fvar_x4, factory) = Factory.create_fvar "X4" factory in
  let (fvar_y1, _factory) = Factory.create_fvar "Y1" factory in
  ((Factory.term_compare fvar_x2 fvar_x4) < 0) &&
  ((Factory.term_compare fvar_y1 fvar_x4) > 0) &&
  ((Factory.term_compare fvar_x4 fvar_x4) = 0)

(* comparison_mvar *)
let%test _ =
  let factory = Factory.empty in
  let (mvar_alpha, factory) = Factory.create_mvar "alpha" factory in
  let (mvar_beta, factory) = Factory.create_mvar "beta" factory in
  let (mvar_gamma, _factory) = Factory.create_mvar "gamma" factory in
  ((Factory.term_compare mvar_alpha mvar_beta) < 0) &&
  ((Factory.term_compare mvar_gamma mvar_beta) > 0) &&
  ((Factory.term_compare mvar_beta mvar_beta) = 0)

(* comparison_app *)
let%test _ =
  let factory = Factory.empty in
  let (mvar_alpha, factory) = Factory.create_mvar "alpha" factory in
  let (mvar_beta, factory) = Factory.create_mvar "beta" factory in
  let (mvar_gamma, factory) = Factory.create_mvar "gamma" factory in
  let (app_f1, factory) = Factory.create_app "f" [mvar_alpha] factory in
  let (app_g2_1, factory) = Factory.create_app "g" [mvar_beta; mvar_gamma] factory in
  let (app_g2_2, factory) = Factory.create_app "g" [mvar_beta; mvar_beta] factory in
  let (app_g3, _factory) = Factory.create_app "g" [mvar_gamma; mvar_alpha; mvar_beta] factory in
  ((Factory.term_compare app_f1 app_g2_1) < 0) &&
  ((Factory.term_compare app_g3 app_g2_1) > 0) &&
  ((Factory.term_compare app_g2_2 app_g2_1) < 0) &&
  ((Factory.term_compare app_g2_1 app_g2_1) = 0)

(* comparison_bind *)
let%test _ =
  let factory = Factory.empty in
  let (mvar_alpha, factory) = Factory.create_mvar "alpha" factory in
  let (mvar_beta, factory) = Factory.create_mvar "beta" factory in
  let (bind_fa, factory) = Factory.create_bind "forall" mvar_alpha factory in
  let (bind_ex_a, factory) = Factory.create_bind "exists" mvar_alpha factory in
  let (bind_ex_b, _factory) = Factory.create_bind "exists" mvar_beta factory in
  ((Factory.term_compare bind_fa bind_ex_b) > 0) &&
  ((Factory.term_compare bind_ex_a bind_ex_b) < 0) &&
  ((Factory.term_compare bind_ex_b bind_ex_b) = 0)

(* comparison_variant *)
let%test _ =
  let factory = Factory.empty in
  let (bvar6, factory) = Factory.create_bvar 6 factory in
  let (fvar_5, factory) = Factory.create_fvar "5" factory in
  let (mvar_4, factory) = Factory.create_mvar "4" factory in
  let (app_3, factory) = Factory.create_app "3" [mvar_4; bvar6; fvar_5] factory in
  let (bind_2, _factory) = Factory.create_bind "2" mvar_4 factory in
  ((Factory.term_compare bvar6 fvar_5) < 0) &&
  ((Factory.term_compare bvar6 mvar_4) < 0) &&
  ((Factory.term_compare bvar6 app_3) < 0) &&
  ((Factory.term_compare bvar6 bind_2) < 0) &&
  
  ((Factory.term_compare fvar_5 mvar_4) < 0) &&
  ((Factory.term_compare fvar_5 app_3) < 0) &&
  ((Factory.term_compare fvar_5 bind_2) < 0) &&
  
  ((Factory.term_compare mvar_4 app_3) < 0) &&
  ((Factory.term_compare mvar_4 bind_2) < 0) &&

  ((Factory.term_compare app_3 bind_2) < 0)

(* Checking access to members of Factory *)
let%test _ =
  (* let _directly_constructed = Bvar 14 in *)
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

  (* let variant_index (term: Factory.term) = (
    match !term with
    | Bvar index -> 0
    | Fvar name -> 1
    | Mvar name -> 2
    | App (name, terms) -> 3
    | Bind (name, _term) -> 4
  ) in *)
  (* Compiler error: The value term has type Factory.term
    but an expression was expected of type 'a ref *)

  let term_to_int term = (
    match (Factory.term_match term) with
    | Bvar index -> 1 + index
    | Fvar name -> 2 + (String.length name)
    | Mvar name -> 3 + (String.length name)
    | App (name, terms) -> 4 + (String.length name) + (List.length terms)
    | Bind (name, _term) -> 5 + (String.length name)
  ) in
  let factory = Factory.empty in
  let (e, _) = Factory.create_mvar "e" factory in
  (term_to_int e) = 4
