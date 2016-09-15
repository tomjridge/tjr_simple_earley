SHELL:=bash

# use make .... ASSERT='' to enable assertions
ASSERT:=-cflag -noassert

OB:=ocamlbuild -use-ocamlfind -tag thread -I src -pkg core $(ASSERT)

all: 
#	$(OB) simple_earley.native 
	$(OB) se_spec.cmo se_spec_all_items.cmo se_simple.cmo se_staged.cmo
	$(OB) se_simple_main.native se_staged_main.native se_test.native

doc: FORCE
	asciidoctor src/e_cn.adoc
	scala README.pre.scala
	asciidoctor README.adoc

FORCE:

CLEAN:=rm -rf a.out *.cmi *.cmo *.cmx *.o *~ _build 

clean:
	-$(CLEAN) && rm -f *.native *.jo *.cmj README.html 
	-cd src && $(CLEAN) && rm -f *.html
	$(OB) -clean
