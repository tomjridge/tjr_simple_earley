all:
	ocamlfind ocamlopt -thread -package core -g -c pervasives_.ml profile.ml staged.ml
	ocamlfind ocamlopt -thread -package core -g -linkpkg -o test.native pervasives_.ml profile.ml staged.ml test.ml
	asciidoctor staged.adoc
	echo Now try: time ./test.native 200
