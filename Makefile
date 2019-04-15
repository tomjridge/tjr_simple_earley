SHELL:=bash
DUNE:=dune

build:
	$(DUNE) build 

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

FORCE:
