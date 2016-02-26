SHELL:=bash

OB:=ocamlbuild -I src

all:
	$(OB) simple_earley.native 
	$(OB) e_cn.cmo e_fg.cmo
	$(OB) e_cn_main.native e_fg_main.native

# pandoc -s simple_earley.md > simple_earley.html
# simple_earley.ml.html: # use htmlfontify-buffer from emacs

CLEAN:=rm -rf a.out *.cmi *.cmo *.cmx *.o *~ _build 

clean:
	-$(CLEAN) && rm -f *.native
	-cd src && $(CLEAN) && rm -f *.html
