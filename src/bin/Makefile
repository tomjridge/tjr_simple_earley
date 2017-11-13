SHELL:=bash
BASH_ENV:=../bash_env.sh
export BASH_ENV

all: 
	$$ocamlopt -linkpkg -package tjr_simple_earley -o test.native test.ml
	$$ocamlopt -linkpkg -package tjr_simple_earley -o simple_test.native simple_test.ml
	echo Now try: time ./test.native 200

