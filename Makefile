SHELL:=bash
#ROOT:=--root=.
DUNE:=dune
MAIN:=bin/earley_main.exe

default:
	$(DUNE) build $(ROOT) @install

all:
	$(DUNE) build $(ROOT) @all
	$(MAKE) docs
#	dune build src/example.exe


# NOTE to promote docs, on command line use: PROMOTE_DOCS=true make
SRC:=_build/default/_doc/_html
DST:=docs/ocamldoc
DST2:=/tmp/tjr_simple_earley
docs: FORCE
	$(DUNE) build $(ROOT) @doc
	@if [ ! -z "$$PROMOTE_DOCS" ]; then rm -rf $(DST)/* ; cp -R $(SRC)/* $(DST); echo "docs built and promoted to docs/"; else \
	  rsync -vaz $(SRC)/* $(DST2); echo "docs built in $(DST2) but not promoted to docs/"; fi

promote_docs: FORCE
	PROMOTE_DOCS=true $(MAKE) docs


run_tests:
	time dune exec $(ROOT) $(MAIN) spec EEE 1111111111
	time dune exec $(ROOT) $(MAIN) unstaged EEE `printf '%*s' 200 | tr ' ' 1` # see NOTE:printf
	time dune exec $(ROOT) $(MAIN) simple 200

# NOTE:printf https://stackoverflow.com/questions/3211891/creating-string-of-repeated-characters-in-shell-script

#	time dune exec $(ROOT) $(MAIN) simple 400 # should take about 6s?

run_examples:
	$(MAKE) run_tests

clean:
	dune clean $(ROOT)


FORCE:
