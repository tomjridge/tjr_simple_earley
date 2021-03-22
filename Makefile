SHELL:=bash
BASH_ENV:=bash_env.sh
export # exports all vars

default: all

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

run_tests:
	$(MAKE)
	time $$run_main spec EEE :1x100
	@echo
	time $$run_main unstaged EEE :1x100
	@echo
	time $$run_main unstaged EEE :1x200
	@echo
	time $$run_main simple EEE :1x100
	@echo
	time $$run_main simple EEE :1x200

run_longer_tests:
	$(MAKE)
	time $$run_main unstaged EEE :1x400
	@echo
	time $$run_main simple EEE :1x400
	@echo

run_actions:
	time $$run_main actions :1x200

run_regression:
	$(MAKE)
	./run_regression.sh
