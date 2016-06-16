{ }:
let 
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
  fetchgit = pkgs.fetchgit;
  op = pkgs.ocamlPackages_latest;
  ocaml = op.ocaml; 
#  findlib= oppkgs.ocamlPackages.findlib;
in
stdenv.mkDerivation {
      name = "simple_earley";
    
      src=./.;
    
      buildInputs = [ ocaml op.findlib op.core_extended op.pa_test ];
    
      configurePhase = "true"; 	# Skip configure
  
      installPhase = "true";

      postInstall="cp -R _build/* $out";
           
      createFindlibDestdir = true;
}