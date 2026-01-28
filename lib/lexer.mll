{
  (* bring token constructors into scope *)
  open Parser

  let keywords = function
    | "type"      -> TYPE
    | "function"  -> FUNCTION
    | "binder"    -> BINDER
    | "rule"      -> RULE
    | "strategy"  -> STRATEGY
    | "where"     -> WHERE
    | "do"        -> DO
    | "depth"     -> DEPTH
    | "limit"     -> LIMIT
    | id           -> IDENT id
}

rule token = parse
  | "\r\n"                    { MenhirLib.LexerUtil.newline lexbuf; token lexbuf }
  | ['\r' '\n']               { MenhirLib.LexerUtil.newline lexbuf; token lexbuf }
  | [' ' '\t']                { token lexbuf }
  | "/*"                      { comment 0 lexbuf }
  | ":"                       { COLON }
  | ";"                       { SEMI }
  | "|"                       { PIPE }
  | "||"                      { PIPEPIPE }
  | ","                       { COMMA }
  | "."                       { DOT }
  | "("                       { LPAREN }
  | ")"                       { RPAREN }
  | "{"                       { LBRACE }
  | "}"                       { RBRACE }
  | "["                       { LBRACKET }
  | "]"                       { RBRACKET }
  | "="                       { EQ }
  | "==>"                     { ARROWBIG }
  | "-->"                     { ARROWDASH }
  | "==X"                     { ARROWX }
  | "->"                      { ARROW }
  | "<-"                      { LARROW }
  | "*"                       { STAR }
  | "?"                       { QUESTION }
  | "@"                       { AT }
  | ['0'-'9']+ as digits      { INT (int_of_string digits) }
  | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']* as id
                              { keywords id }
  | eof                       { EOF }
  | _ as c                    { failwith ("Unexpected char: " ^ String.make 1 c) }

and comment depth = parse
  | "/*"                      { comment (depth + 1) lexbuf }
  | "*/"                      {
      if depth = 0 then token lexbuf else comment (depth - 1) lexbuf
    }
  | "\r\n"                    { MenhirLib.LexerUtil.newline lexbuf; comment depth lexbuf }
  | ['\r' '\n']               { MenhirLib.LexerUtil.newline lexbuf; comment depth lexbuf }
  | eof                       { EOF }
  | _                         { comment depth lexbuf }
