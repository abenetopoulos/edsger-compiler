lexer: lexer.ml
	ocamlopt $< -o lexer

lexer.ml: lexer.mll
	ocamllex $<

parser.ml : parser.mly
	menhir -v $<

clean:
	$(RM) lexer.cm* lexer.o parser.mli parser.automaton parser.conflicts

distclean: clean
	$(RM) lexer lexer.ml
