(* pgeon/lib $ ocamlc factory.ml term_symbol.ml pstring.ml term_index.ml demo/term_index_demo.ml -o term_index_demo.exe *)

open Term_index

type index_fold = {
  index : Term_index.t;
  term : Factory.term;
}

let _ =
  let make_map (list: ('a * 'b) list) =
    let sequence: ('a * 'b) Seq.t = List.to_seq list in
    SymbolKeyedMap.of_seq sequence
  in

  let index_array_node_make (nodes: index_map_node list) =
    let pair i x = (i, x) in
    let sequence = Seq.mapi pair (List.to_seq nodes) in
    SparseArray.of_seq sequence
  in

  let factory0 = Factory.empty in
  let (a, factory1) = Factory.create_app "a" [] factory0 in
  let (b, factory2) = Factory.create_app "b" [] factory1 in
  let (c, factory3) = Factory.create_app "c" [] factory2 in
  let (x, factory4) = Factory.create_fvar "*" factory3 in
  let (g1, factory5) = Factory.create_app "g" [a; x] factory4 in
  let (g2, factory6) = Factory.create_app "g" [x; b] factory5 in
  let (g3, factory7) = Factory.create_app "g" [a; b] factory6 in
  let (g4, factory8) = Factory.create_app "g" [x; c] factory7 in
  let (f1, factory9) = Factory.create_app "f" [g1; c] factory8 in
  let (f2, factory10) = Factory.create_app "f" [g2; x] factory9 in
  let (f3, factory11) = Factory.create_app "f" [g3; c] factory10 in
  let (f4, factory12) = Factory.create_app "f" [g4; b] factory11 in
  let (f5, factory13) = Factory.create_app "f" [x; x] factory12 in
  let terms = [f1; f2; f3; f4; f5] in

  let manual_index = {
    root = make_map [
      (Term_symbol.of_term f1,
        SubArray (
          index_array_node_make [
            make_map [
              (Term_symbol.of_term x,
                SubLeaf (TermSet.of_list [f5])
              );
              (Term_symbol.of_term g1,
                SubArray (
                  index_array_node_make [
                    make_map [
                      (Term_symbol.of_term x,
                        SubLeaf (TermSet.of_list [f2; f4])
                      );
                      (Term_symbol.of_term a,
                        SubLeaf (TermSet.of_list [f1; f3])
                      );
                    ];
                    make_map [
                      (Term_symbol.of_term b,
                        SubLeaf (TermSet.of_list [f2; f3])
                      );
                      (Term_symbol.of_term c,
                        SubLeaf (TermSet.of_list [f4])
                      );
                      (Term_symbol.of_term x,
                        SubLeaf (TermSet.of_list [f1])
                      );
                    ];
                  ]
                )
              );
            ];
            make_map [
              (Term_symbol.of_term b,
                SubLeaf (TermSet.of_list [f4])
              );
              (Term_symbol.of_term c,
                SubLeaf (TermSet.of_list [f1; f3])
              );
              (Term_symbol.of_term x,
                SubLeaf (TermSet.of_list [f2; f5])
              );
            ];
          ];
        )
      )
    ]
  } in
  Printf.printf "manual_index: %s\n\n" (Term_index.string_of manual_index);

  let procedural_index1 = Term_index.empty in

  let print_insertions fold pstring =
    let index = fold.index in
    let term = fold.term in
    Printf.printf "pstring: %s\n" (Pstring.string_of pstring);
    let new_index = Term_index.add index pstring term in
    Printf.printf "procedural_index: %s\n" (Term_index.string_of new_index);
    { fold with index = new_index }
  in

  let print_insertions_many current_index term =
    Printf.printf "\nterm: %s (%s)\n" (Factory.string_of_term term) (Factory.string_address_of term);
    (* let pstrings = Pstring.all_of_term term in *)
    (* let initial = { index = current_index; term = term } in *)
    let new_index = Term_index.add_term current_index term in
    Printf.printf "procedural_index: %s\n" (Term_index.string_of new_index);
    (* let new_index = (List.fold_left print_insertions initial pstrings).index in *)
    Printf.printf "\n";
    new_index
  in

  let print_deletions fold pstring =
    let index = fold.index in
    let term = fold.term in
    Printf.printf "pstring: %s\n" (Pstring.string_of pstring);
    let new_index = Term_index.remove index pstring term in
    Printf.printf "procedural_index: %s\n" (Term_index.string_of new_index);
    { fold with index = new_index }
  in

  let print_deletions_many current_index term =
    Printf.printf "\nterm: %s (%s)\n" (Factory.string_of_term term) (Factory.string_address_of term);
    (* let pstrings = Pstring.all_of_term term in
    let initial = { index = current_index; term = term } in *)
    let new_index = Term_index.remove_term current_index term in
    Printf.printf "procedural_index: %s\n" (Term_index.string_of new_index);
    (* let new_index = (List.fold_left print_deletions initial pstrings).index in *)
    Printf.printf "\n";
    new_index
  in

  let procedural_index2 = List.fold_left print_insertions_many procedural_index1 terms in
  Printf.printf "\n\nDeletions:\n\n";
  let _ = List.fold_left print_deletions_many procedural_index2 terms in
  ()
  ;;
