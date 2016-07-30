type token =
  | IDE of (string)
  | NUM of (int)
  | REAL of (float)
  | TRUE
  | FALSE
  | PROGRAM
  | VAR
  | ARRAY
  | OF
  | INT
  | FLOAT
  | PROCEDURE
  | FUNCTION
  | BEGIN
  | END
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | REPEAT
  | UNTIL
  | FOR
  | TO
  | WRITE
  | CALL
  | PLUS
  | MINUS
  | TIMES
  | DIVISION
  | EQUAL
  | LESSEQUAL
  | LESS
  | AND
  | OR
  | NOT
  | ASSIGN
  | SEMICOLON
  | COLON
  | COMMA
  | LS
  | RS
  | LP
  | RP
  | GREATEREQUAL
  | GREATER
  | RECORD
  | DOT
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntaxtree.program
