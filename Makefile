lexer: lexer.ml
	ocamlopt $< -o lexer

lexer.ml: lexer.mll
	ocamllex $<

clean:
	$(RM) lexer.cm* lexer.o

distclean: clean
	$(RM) lexer lexer.ml
