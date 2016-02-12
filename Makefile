SHELL:=bash

all:
	ocamlopt simple_earley.ml spec.ml
	pandoc -s simple_earley.md > simple_earley.html
	cd src && ocamlopt e_common.ml e_bc.ml e_cd.ml

# simple_earley.ml.html: # use htmlfontify-buffer from emacs

CLEAN:=rm -f a.out *.cmi *.cmo *.cmx *.o *~

clean:
	-$(CLEAN)
	-cd src && $(CLEAN)
