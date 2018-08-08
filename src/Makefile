SHELL:=bash
BASH_ENV:=bash_env.sh
export BASH_ENV


all: 
	$(MAKE) build
	$(MAKE) install
	$(MAKE) -C bin

build:
	$$ocamlc -c $$mls
	$$ocamlopt -c $$mls
	$$mk_cma -g -a -o $$libname.cma $$cmos
	$$mk_cmxa -g -a -o $$libname.cmxa $$cmxs

install:
	-ocamlfind remove $$package_name
	ocamlfind install $$package_name META *.cmi *.cmo *.o *.a *.cma *.cmxa *.cmx 

update_config:
	ocaml <config.ml_

# apt-install ruby-pygments
# this requires snip.native, which is unreleased ATM
snips: FORCE
	/tmp/l/bin/snip.native tjr_earley.ml

clean:
	rm -f *.cmi *.cmo *.cmx *.o *.x *.a *.cma *.cmxa
	find . -xtype l -exec rm -f \{\} \;
	$(MAKE) -C bin clean


FORCE:
