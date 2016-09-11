SHELL:=bash

# use make .... ASSERT='' to enable assertions
ASSERT:=-cflag -noassert

OB:=ocamlbuild -use-ocamlfind -tag thread -I src -pkg core $(ASSERT)

all: 
#	$(OB) simple_earley.native 
	$(OB) e_bc.cmo e_cd.cmo e_cn.cmo e_fg.cmo
	$(OB) e_cn_main.native e_fg_main.native e_test.native

doc: FORCE
	scala README.pre.scala
	asciidoctor README.adoc

FORCE:

CLEAN:=rm -rf a.out *.cmi *.cmo *.cmx *.o *~ _build 

clean:
	-$(CLEAN) && rm -f *.native *.jo *.cmj README.html 
	-cd src && $(CLEAN) && rm -f *.html
