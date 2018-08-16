SHELL:=bash
BASH_ENV:=./bash_env.sh
export BASH_ENV

all: 
	$$ocamlopt -linkpkg -o unstaged.native unstaged.ml
	echo Now try: time ./unstaged.native 200


clean:
	clean

