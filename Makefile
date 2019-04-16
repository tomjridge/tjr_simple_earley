SHELL:=bash
ROOT:=--root=.
DUNE:=dune

all:
	$(DUNE) build $(ROOT) @all
	$(MAKE) docs
#	dune build src/example.exe


SRC:=_build/default/_doc/_html
DST:=docs
docs: FORCE
	$(DUNE) build $(ROOT) @doc
	rm -rf $(DST)/*
	cp -R $(SRC)/* $(DST)
	echo "docs built"


run_example:
	time dune exec $(ROOT) src/example.exe 400 # should take about 6s?

clean:
	dune clean $(ROOT)


FORCE:
