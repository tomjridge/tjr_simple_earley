dune_build="dune build"
dune_exec="dune exec"
run_main="dune exec bin/earley_main.exe"

# 
# maybe_promote_docs() {
#     SRC=_build/default/_doc/_html
#     DST=docs/ocamldoc
#     DST2=/tmp/tjr_simple_earley
# 	  if [ ! -z "$PROMOTE_DOCS" ]; then 
#         rm -rf $DST/*
#         cp -Rv $SRC/* $DST
#         echo "docs in $SRC promoted to $DST"
#     else 
# 	      rsync -vaz $SRC/* $DST2
#         echo "docs built in $DST2 but not promoted to docs/"
#     fi
# }
