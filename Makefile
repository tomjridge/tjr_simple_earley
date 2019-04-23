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
	$(MAKE)
	time dune exec $(ROOT) $(MAIN) spec EEE :1x10
	time dune exec $(ROOT) $(MAIN) unstaged EEE :1x400
	time dune exec $(ROOT) $(MAIN) simple :1x400


run_examples:
	$(MAKE) run_tests


run_regression:
	$(MAKE)
	./run_regression.sh


disable_log:
	echo "let log (x:unit Lazy.t) = ()" >src/log.ml

enable_log:
	echo "let log (x:unit Lazy.t) = Lazy.force x" >src/log.ml

clean:
	dune clean $(ROOT)


FORCE:
