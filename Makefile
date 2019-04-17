SHELL:=bash
#ROOT:=--root=.
DUNE:=dune

all:
	$(DUNE) build $(ROOT) @all
	$(MAKE) docs
#	dune build src/example.exe


# NOTE to promote docs, on command line use: PROMOTE_DOCS=true make
SRC:=_build/default/_doc/_html
DST:=docs
docs: FORCE
	$(DUNE) build $(ROOT) @doc
	@if [ ! -z "$$PROMOTE_DOCS" ]; then rm -rf $(DST)/* ; cp -R $(SRC)/* $(DST); echo "docs built and promoted to docs/"; else \
	  echo "docs built but not promoted to docs/"; fi


run_example:
	time dune exec $(ROOT) src/example.exe 400 # should take about 6s?

clean:
	dune clean $(ROOT)


FORCE:
