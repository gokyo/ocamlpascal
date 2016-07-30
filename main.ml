open Syntaxtree;;
open Lexer;;
open Parser;;
open Interpreter;;

let lexbuf = Lexing.from_channel stdin in
let absyntaxtree = program lex lexbuf in
	print_string ">>> The program is syntactically correct!\n";
	run absyntaxtree;;
