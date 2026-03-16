type formula = int * Term.t (* id, term *)

type proof_tree =
  formula list list (* list of branches, each branch is a list of terms *)

let is_closed = function [] -> true | _ -> false

type application_key = int * int list (* rule id, sorted list of formula ids *)

type proof_state = {
  tree : proof_tree;
  next_fresh : int;
  next_symbol : int;
  next_formula_id : int;
  applied : application_key list; (* cache of applied rules per formula *)
}

type strategy = proof_state -> proof_state Seq.t

type rule = {
  id : int;
  run : proof_state -> proof_state Seq.t;
  run_bang : (proof_state -> proof_state option) option;
}

(* let fair_flat_map (f : 'a -> 'b Seq.t) (xs : 'a Seq.t) : 'b Seq.t = *)
(*   let rec pull_children (parents : 'a Seq.t) (children : 'b Seq.t list) () = *)
(*     match children with *)
(*     | child :: rest -> ( *)
(*         match child () with *)
(*         | Seq.Nil -> *)
(*             pull_children parents rest () *)
(*         | Seq.Cons (y, child') -> *)
(*             Seq.Cons (y, expand_parents parents (rest @ [child']))) *)
(*     | [] -> *)
(*         expand_parents parents [] () *)
(*   and expand_parents (parents : 'a Seq.t) (children : 'b Seq.t list) () = *)
(*     match parents () with *)
(*     | Seq.Nil -> *)
(*         pull_children Seq.empty children () *)
(*     | Seq.Cons (x, parents') -> *)
(*         pull_children parents' (children @ [f x]) () *)
(*   in *)
(*   expand_parents xs [] *)

let fair_flat_map (f : 'a -> 'b Seq.t) (xs : 'a Seq.t) : 'b Seq.t =
  let children : 'b Seq.t Queue.t = Queue.create () in
  let rec step (parents : 'a Seq.t) () =
    (* Enqueue at most one new child stream from parents *)
    let parents =
      match parents () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons (x, parents') ->
          Queue.push (f x) children;
          parents'
    in
    pull parents ()
  and pull (parents : 'a Seq.t) () =
    if Queue.is_empty children then (
      (* No active children: try to get more parents *)
      match parents () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (x, parents') ->
          Queue.push (f x) children;
          pull parents' ())
    else
      let child = Queue.pop children in
      match child () with
      | Seq.Nil ->
          (* dead child, skip it *)
          pull parents ()
      | Seq.Cons (y, child') ->
          (* emit one result, rotate child tail to the back *)
          Queue.push child' children;
          Seq.Cons (y, step parents)
  in
  step xs

(* combinators *)
let skip = fun st -> Seq.return st
let fail = fun _ -> Seq.empty

(* let orElse s1 s2 = fun st -> Seq.append (s1 st) (s2 st) *)
let orElse s1 s2 =
 fun st ->
  let xs = s1 st in
  match xs () with Seq.Nil -> s2 st | Seq.Cons _ -> xs

let andThen s1 s2 = fun st -> Seq.flat_map s2 (s1 st)
let orAlt s1 s2 = fun st -> Seq.interleave (s1 st) (s2 st)
let andAlt s1 s2 = fun st -> fair_flat_map s2 (s1 st)
let depth = ref 0

let rec repeat s st () =
  depth := !depth + 1;
  Seq.Cons (st, fair_flat_map (repeat s) (s st))

let applyRule r = fun st -> r.run st

let applyRuleBang r =
 fun st ->
  match r.run_bang with
  | None -> failwith "Rule does not support bang application"
  | Some f -> (
      match f st with None -> Seq.empty | Some st' -> Seq.return st')

(* main engine *)
let prove st s = Seq.exists (fun st -> is_closed st.tree) (s st)
