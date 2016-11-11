type token =
  | LID of (string)
  | CID of (string)
  | OID of (string)
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | TICK
  | UNDERSCORE
  | QMARK
  | COLON
  | ARROW
  | STAR
  | COMMA
  | AS
  | OF
  | AND
  | EXCEPTION
  | MODULE
  | TYPE
  | VAL
  | LT
  | GT
  | EQ
  | ELLIPSIS
  | SEMICOLON
  | SHARP
  | DOT
  | OR
  | BACKTICK
  | EOF
  | SIG
  | END

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Specification.t list
