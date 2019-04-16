SHELL:=bash

all:
	dune build @all
	dune build @doc-private
	dune build src/example.exe
	echo "doc at _build/default/_doc/_html//xxx@./Earley/index.html"

run_example:
	time dune exec src/example.exe 400 # should take about 6s?

clean:
	dune clean


