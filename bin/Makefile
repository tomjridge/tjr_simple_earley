SHELL:=bash
BASH_ENV:=./bash_env.sh
export BASH_ENV

all: 
	$$ocamlopt -linkpkg -o test.native test.ml
	$$ocamlopt -linkpkg -o test2.native test2.ml
	$$ocamlopt -linkpkg -o simple_test.native simple_test.ml
	echo Now try: time ./test.native 200


clean:
	clean

