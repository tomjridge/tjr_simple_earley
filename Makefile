SHELL:=bash
DUNE:=dune

build:
	$(DUNE) build @install
	$(DUNE) build bin/simple_test.exe bin/test.exe bin/test2.exe

# NOTE install and uninstall do not involve opam; to build and install with opam, first pin
install: 
	$(DUNE) install

uninstall: 
	$(DUNE) uninstall

clean: 
	$(DUNE) clean

all:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) install
	$(MAKE) docs

SRC:=_build/default/_doc/_html
DST:=docs
docs: FORCE
	$(DUNE) build @doc
	rm -rf $(DST)/*
	cp -R $(SRC)/* $(DST)

run_examples:
	time $(DUNE) exec bin/simple_test.exe 400
	time $(DUNE) exec bin/test.exe 400
	time $(DUNE) exec bin/test2.exe 400

FORCE:
