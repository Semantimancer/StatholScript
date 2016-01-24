type token =
  | FLOAT of (float)
  | BOOL of (bool)
  | CHAR of (char)
  | AOP of (char)
  | BOP of (string)
  | COP of (string)
  | ID of (string)
  | NOT
  | IF
  | THEN
  | ELSE
  | FST
  | SND
  | HEAD
  | TAIL
  | NULL
  | CONS
  | LET
  | EQUAL
  | EQUALITY
  | IN
  | EOL
  | ARROW
  | LAMBDA
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | COMMA
  | MINUS
  | SEQUENCE

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.resultS
