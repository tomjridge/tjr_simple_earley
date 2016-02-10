SHELL:=bash

all:
	ocamlopt simple_earley.ml spec.ml
	pandoc -s simple_earley.md > simple_earley.html

# simple_earley.ml.html: # use htmlfontify-buffer from emacs

clean:
	-rm -f a.out *.cmi *.cmo *.cmx *.o
