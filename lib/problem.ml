open Ast

type t = {
  functions : function_decl list;
  formulas : expr list;
}

let of_components functions formulas = { functions; formulas }

let function_names t = List.map (fun (fd : function_decl) -> fd.name) t.functions

let symbol_fvar (problem : t) =
  let rec aux env acc = function
    | LVar n ->
        if List.mem n env || List.mem n acc then acc else n :: acc
    | LFun (_, el) -> List.fold_left (aux env) acc el
    | LBinder (_, n, e) -> aux (n :: env) acc e
  in
  List.fold_left (aux []) [] problem.formulas
