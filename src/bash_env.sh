
set -a # export all vars

libname="tjr_simple_earley"
package_name="tjr_simple_earley"

ocamlc="ocamlfind ocamlc -g   "
ocamlopt="ocamlfind ocamlopt -g   "

mls="profile_waypoints.ml profile.ml tjr_earley.ml"
cmos="profile_waypoints.cmo profile.cmo tjr_earley.cmo"
cmxs="profile_waypoints.cmx profile.cmx tjr_earley.cmx"

mk_cmxa="ocamlfind ocamlopt -g   "

