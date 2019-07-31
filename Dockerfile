
FROM ocaml/opam2:4.07

# some of the following apt packages are likely already installed
RUN sudo apt-get install -y git make
RUN sudo apt-get install -y curl
RUN sudo apt-get install -y gcc
RUN sudo apt-get install -y bzip2
RUN sudo apt-get install -y wget
RUN sudo apt-get install -y unzip m4
RUN sudo apt-get install -y time
RUN sudo apt-get install -y rsync bubblewrap

RUN opam update

# install some common packages, so they are cached in future docker builds
RUN opam install dune ocamlfind odoc
RUN opam install core_kernel 
RUN opam install core
RUN opam install re

# drop the RUN prefix from the following lines (and ignore previous lines!)
# to build using local opam install

RUN opam pin add -y -n https://github.com/tomjridge/tjr_lib.git
RUN opam pin add -y -n https://github.com/tomjridge/tjr_profile.git
RUN opam pin add -y -n https://github.com/tomjridge/tjr_simple_earley.git#dev
RUN opam pin add -y -n tjr_simple_earley_examples https://github.com/tomjridge/tjr_simple_earley.git#dev
RUN opam install -y tjr_simple_earley
