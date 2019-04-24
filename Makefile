SHELL:=bash
BASH_ENV:=bash_env.sh
export # exports all vars

default:
	$$dune_build @install

all:
	$$dune_build @all
	$(MAKE) docs
#	dune build src/example.exe


# NOTE to promote docs, on command line use: PROMOTE_DOCS=true make
docs: FORCE
	$$dune_build @doc
	maybe_promote_docs

promote_docs: FORCE
	PROMOTE_DOCS=true $(MAKE) docs


run_examples:
	$(MAKE) run_tests

run_tests:
	$(MAKE)
	time $$run_main spec EEE :1x100
	@echo
	time $$run_main unstaged EEE :1x100
	@echo
	time $$run_main simple EEE :1x100

run_longer_tests:
	$(MAKE)
	time $$run_main unstaged EEE :1x400
	@echo
	time $$run_main simple EEE :1x400
	@echo

tmp:
	OCAMLRUNPARAM="O=1000000,a=1,s=1000000,i=100,o=99" time $$run_main simple :1x400
	OCAMLRUNPARAM="O=1000000,a=1,s=1000000,i=100,o=99" time $$run_main unstaged EEE :1x400
# NOTE OCAMLRUNPARAM variations don't seem to make much of a difference

run_regression:
	$(MAKE)
	./run_regression.sh


disable_log:
	echo "let log (x:unit Lazy.t) = ()" >src/log.ml

enable_log:
	echo "let log (x:unit Lazy.t) = Lazy.force x" >src/log.ml

clean:
	dune clean 

clean_docs:
	rm -rf docs/ocamldoc/*

FORCE:
