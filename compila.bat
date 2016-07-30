
        del parser.mli parser.ml lexer.ml *.cmo *.cmi interpreter interpreter.exe *~


	ocamlc     -c syntaxtree.ml
	ocamlyacc  -v parser.mly
	ocamlc     -c parser.mli
	ocamlc     -c parser.ml
	ocamllex      lexer.mll
	ocamlc     -c lexer.ml
	ocamlc     -c interpreter.ml
	ocamlc     -c main.ml
	ocamlc     -o interpreter.exe lexer.cmo parser.cmo syntaxtree.cmo interpreter.cmo main.cmo


pause
