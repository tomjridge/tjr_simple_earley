
set -a # export all vars

libname="tjr_simple_earley"
package_name="tjr_simple_earley"

ocamlc="ocamlfind ocamlc -g -w @f@p@u@s@40-8-11-26  "
ocamlopt="ocamlfind ocamlopt -g -w @f@p@u@s@40-8-11-26  "

mls="profile_waypoints.ml profile.ml tjr_earley.ml"
cmos="profile_waypoints.cmo profile.cmo tjr_earley.cmo"
cmxs="profile_waypoints.cmx profile.cmx tjr_earley.cmx"

mk_cmxa="ocamlfind ocamlopt -g -w @f@p@u@s@40-8-11-26  "


function clean() {
  rm -f *.cmi *.cmo *..cmx *.o *.x *.a *.cma *.cmxa
	find . -xtype l -exec rm -f \{\} \;
}


