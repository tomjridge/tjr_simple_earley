ASS:=-noassert
MLS:=$(shell ocamldep -one-line -sort *.ml)
OPT:=ocamlfind ocamlopt $(ASS) -thread -package core -g

all: 
	$(OPT) -c $(MLS)
	$(OPT) -linkpkg -o test.native $(MLS)
	echo Now try: time ./test.native 200

doc:
	asciidoctor staged.adoc

clean:
	rm -f *.{cmi,cmo,cmx,o,cmt} a.out *.cma *.cmxa *.a *.byte *.native
