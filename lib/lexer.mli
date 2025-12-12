val keywords : string -> Parser.token
val __ocaml_lex_tables : Lexing.lex_tables
val token : Lexing.lexbuf -> Parser.token
val __ocaml_lex_token_rec : Lexing.lexbuf -> int -> Parser.token
val comment : int -> Lexing.lexbuf -> Parser.token
val __ocaml_lex_comment_rec :
  int -> Lexing.lexbuf -> int -> Parser.token
