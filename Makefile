SHELL:=bash

all:
	pandoc -s simple_earley.md > simple_earley.html
	ocamlopt simple_earley.ml

# simple_earley.ml.html: # use htmlfontify-buffer from emacs

clean:
	-rm -f a.out *.cmi *.cmo *.cmx *.o
