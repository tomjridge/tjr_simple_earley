SHELL:=bash
BASH_ENV:=../bash_env.sh
export BASH_ENV

all: 
	$$ocamlopt -linkpkg -package tjr_simple_earley -g -o test.native test.ml
	$$ocamlopt -linkpkg -package tjr_simple_earley -g -o test2.native test2.ml
	$$ocamlopt -linkpkg -package tjr_simple_earley -g -o simple_test.native simple_test.ml
	echo Now try: time ./test.native 200


clean:
	rm -f *.cmi *.cmo *.cmx *.o *.x *.a *.cma *.cmxa *.native

