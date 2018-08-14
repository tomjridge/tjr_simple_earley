ocamlopt="ocamlfind ocamlopt -thread -package tjr_monad,tjr_simple_earley,tjr_profile,core -g"

function clean() {
	rm -f *.cmi *.cmo *.cmx *.o *.x *.a *.cma *.cmxa *.native
}


