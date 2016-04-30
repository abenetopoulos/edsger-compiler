all:
	menhir -v parser.mly
	ocamllex lexer.mll
	ocamlopt -c parser.mli
	ocamlopt -c parser.ml
	ocamlopt -c lexer.ml
	ocamlopt -c main.ml
	ocamlopt parser.cmx lexer.cmx main.cmx -o edsgerc

lexer.ml: lexer.mll
	ocamllex $<

parser.cmi : parser.ml
	ocamlopt -c $<

parser.ml : parser.mly
	menhir -v $<

%.cmi: %.mli
	ocamlopt -c $<

%.cmo: %.ml
	ocamlopt -c $<

clean:
	rm *.cmi *.mli *.cmx *.automaton *.conflicts *.o parser.ml 

distclean: clean
	rm edsgerc lexer.ml
