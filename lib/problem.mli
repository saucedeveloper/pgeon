type t = {
  functions : Ast.function_decl list;
  formulas : Ast.expr list;
}

val of_components : Ast.function_decl list -> Ast.expr list -> t
val function_names : t -> string list
val symbol_fvar : t -> string list
