{ (* HEADER *)

open Parser;;

exception UnknownChar;;

}

(* REGULAR DEFINITIONS *)

let integer = '-'? ['0'-'9']+
let float   = '-'? ['0'-'9']* '.' ['0'-'9']+
let ident   = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* RULES *)

rule lex =
  parse integer         { NUM (int_of_string (Lexing.lexeme lexbuf))    }
      | float           { REAL (float_of_string (Lexing.lexeme lexbuf)) }

      | "true"          { TRUE }
      | "false"         { FALSE }


      | "."             { DOT }
      | "record"        { RECORD }


      | "program"       { PROGRAM }
      | "var"           { VAR }
      | "array"         { ARRAY }
      | "of"            { OF }
      | "int"           { INT }
      | "float"         { FLOAT }
      | "procedure"     { PROCEDURE }
      | "function"      { FUNCTION }
      | "begin"         { BEGIN }
      | "end"           { END }
      | "if"            { IF }
      | "then"          { THEN }
      | "else"          { ELSE }
      | "while"         { WHILE }
      | "do"            { DO }
      | "repeat"        { REPEAT }
      | "until"         { UNTIL }
      | "for"           { FOR }
      | "to"            { TO }
      | "write"         { WRITE }
      | "call"          { CALL }

      | ident           { IDE (Lexing.lexeme lexbuf) }

      | "+"             { PLUS }
      | "-"             { MINUS }
      | "*"             { TIMES }
      | "/"             { DIVISION }
      | "="             { EQUAL }
      | "<="            { LESSEQUAL }
      | "<"             { LESS }

      | ">="            { GREATEREQUAL } 
      | ">"             { GREATER }

      | "&"             { AND }
      | "|"             { OR }
      | "!"             { NOT }
      | ":="            { ASSIGN }

      | ";"             { SEMICOLON }
      | ":"             { COLON }
      | ","             { COMMA }

      | "("             { LP }
      | ")"             { RP }
      | "["             { LS }
      | "]"             { RS }




      | [' ' '\t' '\n'] { lex lexbuf }
      | eof             { EOF }

      | _               { raise UnknownChar }

