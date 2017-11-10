SHELL:=bash
ASS:=-noassert
MLS:=profile.ml tjr_earley.ml # $(shell ocamldep -one-line -sort *.ml)
OPT:=ocamlfind ocamlopt $(ASS) -thread -package core -g

all: 
#	$(OPT) -c $(MLS)
	$(OPT) -linkpkg -o test.native $(MLS) test.ml
	$(OPT) -linkpkg -o simple_test.native $(MLS) simple_test.ml
	echo Now try: time ./test.native 200


# apt-install ruby-pygments
doc:
	/tmp/snip.native tjr_earley.ml
	asciidoctor tjr_earley.adoc

clean:
	rm -f *.{cmi,cmo,cmx,o,cmt} a.out *.cma *.cmxa *.a *.byte *.native
