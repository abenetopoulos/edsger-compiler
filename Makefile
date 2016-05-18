OCAMLC=ocamlc -g
OCAMLOPT=ocamlopt -g
COMPFLAGS= -g
CAMLP5_FLAGS=-pp "camlp5o ./extend.cmo"

all:
	make semantic
	menhir -v parser.mly
	ocamllex lexer.mll
	ocamlopt -c parser.mli
	ocamlopt -c parser.ml
	ocamlopt -c lexer.ml
	ocamlopt -c main.ml
	ocamlopt ast.cmx Error.cmx Hashcons.cmx Identifier.cmx Types.cmx Symbol.cmx semantic.cmx parser.cmx lexer.cmx main.cmx -o edsgerc

depend:
	make extend
	make error
	make hashc
	make identifier
	make types
	make symbol
	make ast

extend: extend.ml
	$(OCAMLC) $(COMPFLAGS) -pp "camlp5o pa_extend.cmo q_MLast.cmo" -I `camlp5 -where` -c $< 
	$(OCAMLOPT) $(COMPFLAGS) -pp "camlp5o pa_extend.cmo q_MLast.cmo" -I `camlp5 -where` -c $< 

error: Error.ml
	$(OCAMLOPT) -i Error.ml > Error.mli
	$(OCAMLOPT) -c Error.mli
	$(OCAMLOPT) -c Error.ml

hashc: Hashcons.ml
	$(OCAMLOPT) -i Hashcons.ml > Hashcons.mli
	$(OCAMLOPT) -c Hashcons.mli
	$(OCAMLOPT) -c Hashcons.ml

identifier: Identifier.ml
	$(OCAMLOPT) -i Identifier.ml > Identifier.mli
	$(OCAMLOPT) -c Identifier.mli
	$(OCAMLOPT) -c Identifier.ml

types: Types.ml
	$(OCAMLOPT) -i Types.ml > Types.mli
	$(OCAMLOPT) -c Types.mli
	$(OCAMLOPT) -c Types.ml

symbol: Symbol.ml extend.cmo
	$(OCAMLOPT) $(CAMLP5_FLAGS) -i Symbol.ml > Symbol.mli
	$(OCAMLOPT) $(CAMLP5_FLAGS) -c Symbol.mli 
	$(OCAMLOPT) $(CAMLP5_FLAGS) -c Symbol.ml

ast: ast.ml
	$(OCAMLOPT) -i ast.ml > ast.mli
	$(OCAMLOPT) -c ast.mli
	$(OCAMLOPT) -c ast.ml

semantic: semantic.ml
	$(OCAMLOPT) -i semantic.ml > semantic.mli
	$(OCAMLOPT) -c semantic.mli
	$(OCAMLOPT) -c semantic.ml


lexer.ml: lexer.mll
	ocamllex $<

parser.cmi : parser.ml
	ocamlopt -c $<

parser.ml : parser.mly
	menhir -v $<

%.cmo: %.ml %.mli extend.cmx
	$(OCAMLOPT) $(CAMLP5_FLAGS) -c $<

%.cmx: %.ml extend.cmx
	$(OCAMLOPT) $(CAMLP5_FLAGS) -c $<

%.cmi: %.mli extend.cmx
	$(OCAMLOPT) $(CAMLP5_FLAGS) -c $<

clean:
	rm *.cmi *.cmo *.mli *.cmx *.automaton *.conflicts *.o parser.ml 

distclean: clean
	rm edsgerc lexer.ml
