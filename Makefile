SHELL:=bash

# use make .... ASSERT='' to enable assertions
ASSERT:=-cflag -noassert

OB:=ocamlbuild -use-ocamlfind -tag thread -I src -pkg core $(ASSERT)

all: doc
	$(OB) simple_earley.native 
	$(OB) e_bc.cmo e_cd.cmo e_cn.cmo e_fg.cmo
	$(OB) e_cn_main.native e_fg_main.native e_test.native

doc: FORCE
	asciidoctor README.adoc

FORCE:

# pandoc -s simple_earley.md > simple_earley.html
# simple_earley.ml.html: # use htmlfontify-buffer from emacs

CLEAN:=rm -rf a.out *.cmi *.cmo *.cmx *.o *~ _build 

clean:
	-$(CLEAN) && rm -f *.native *.jo *.cmj README.html
	-cd src && $(CLEAN) && rm -f *.html
