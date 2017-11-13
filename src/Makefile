SHELL:=bash
BASH_ENV:=bash_env.sh
export BASH_ENV

all: 
	echo "NOTE before anything, run ocaml <config.ml_"
	$$ocamlopt -c $$mls
	$$mk_cmxa -g -a -o $$libname.cmxa $$cmxs
	$(MAKE) install
	$(MAKE) -C bin

install:
	-ocamlfind remove $$package_name
	ocamlfind install $$package_name META *.cmi *.o *.a *.cmxa *.cmx 


# apt-install ruby-pygments
# this requires snip.native, which is unreleased ATM
doc:
	/tmp/l/bin/snip.native tjr_earley.ml
	asciidoctor tjr_earley.adoc
	cp tjr_earley.html ../docs

clean:
	clean

FORCE:
