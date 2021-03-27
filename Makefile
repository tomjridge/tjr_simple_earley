SHELL:=bash
# BASH_ENV:=bash_env.sh
# export # exports all vars

default: all 

all:: 
	$(MAKE) earley_main.exe

earley_main.exe: 
	dune build bin/earley_main.exe
	cp _build/default/bin/earley_main.exe .

-include Makefile.ocaml

clean::

run_all: 
	$(MAKE) run_tests
	$(MAKE) run_longer_tests
	$(MAKE) run_examples
	$(MAKE) run_actions
	$(MAKE) run_regression

run_examples:
	$(MAKE) run_tests

run_2021:
	$(MAKE)
	time ./earley_main.exe earley_2021q1 :1x100
	time ./earley_main.exe earley_2021q1 :1x200
	time ./earley_main.exe earley_2021q1 :1x300
	time ./earley_main.exe earley_2021q1 :1x400
# see https://gist.github.com/tomjridge/36ec17d16035768ee0212377b66e42e5 for timings

run_tests:
	$(MAKE)
	time ./earley_main.exe earley_2021q1 :1x100
	@echo
	time ./earley_main.exe spec EEE :1x100
	@echo
	time ./earley_main.exe unstaged EEE :1x100
	@echo
	time ./earley_main.exe unstaged EEE :1x200
	@echo
	time ./earley_main.exe simple EEE :1x100
	@echo
	time ./earley_main.exe simple EEE :1x200

run_longer_tests:
	$(MAKE)
	time ./earley_main.exe unstaged EEE :1x400
	@echo
	time ./earley_main.exe simple EEE :1x400
	@echo

run_actions:
	time ./earley_main.exe actions :1x200

run_regression:
	$(MAKE)
	./run_regression.sh
