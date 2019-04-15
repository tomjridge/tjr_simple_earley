SHELL:=bash
BASH_ENV:=./bash_env.sh
export BASH_ENV

all: 
	$$ocamlopt -linkpkg -o leo.native leo.ml
	echo Now try: time ./leo.native 200


clean:
	clean

