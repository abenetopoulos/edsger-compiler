all:
	menhir -v parser_experimental.mly
	ocamllex lexer.mll
	ocamlopt -c parser_experimental.mli
	ocamlopt -c parser_experimental.ml
	ocamlopt -c lexer.ml
	ocamlopt -c main.ml
	ocamlopt parser_experimental.cmx lexer.cmx main.cmx -o main

lexer.ml: lexer.mll
	ocamllex $<

parser.cmi : parser.ml
	ocamlopt -c $<

parser.ml : parser.mly
	menhir -v $<

parser_experimental.cmi : parser_experimental.ml
	ocamlopt -c $<

parser_experimental.ml : parser_experimental.mly
	menhir -v $<

%.cmi: %.mli
	ocamlopt -c $<

%.cmo: %.ml
	ocamlopt -c $<

#%.cmx: %.ml
#	$(OCCOPT) $(OCC_FLAGS) $<

clean:
	rm *.cmi *.mli *.cmx *.automaton *.conflicts *.o parser_experimental.ml

distclean: clean
	rm main lexer lexer.ml
