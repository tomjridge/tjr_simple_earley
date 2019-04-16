SHELL:=bash
DUNE:=dune
ROOT:=--root=.

build:
	$(DUNE) build $(ROOT) @install
	$(DUNE) build $(ROOT) bin/simple_test.exe bin/test.exe bin/test2.exe

# NOTE install and uninstall do not involve opam; to build and install with opam, first pin
install: 
	$(DUNE) install $(ROOT) 

uninstall: 
	$(DUNE) uninstall $(ROOT) 

clean: 
	$(DUNE) clean $(ROOT) 

all:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) install
	$(MAKE) docs

SRC:=_build/default/_doc/_html
DST:=docs
docs: FORCE
	$(DUNE) build $(ROOT) @doc
	rm -rf $(DST)/*
	cp -R $(SRC)/* $(DST)

run_examples:
	time $(DUNE) exec $(ROOT) bin/simple_test.exe 400
	time $(DUNE) exec $(ROOT) bin/test.exe 400
	time $(DUNE) exec $(ROOT) bin/test2.exe 400

FORCE:
