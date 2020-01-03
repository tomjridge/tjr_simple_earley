(lang dune 2.0)
(name tjr_simple_earley)

;; for opam
(authors "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers "Tom Ridge <tom.j.ridge@googlemail.com>")
(source (github tomjridge/tjr_simple_earley))
(homepage https://github.com/tomjridge/tjr_simple_earley)
(documentation https://tomjridge.github.io/tjr_simple_earley/ocamldoc/index.html)
(generate_opam_files true)


(package 
         (name tjr_simple_earley)
         (synopsis "Simple Earley-style parser")
         (depends ocamlfind dune ppx_jane 
                  core ;; for profiling, in bin
                   tjr_profile ;; for profiling, in bin
                   ppx_include)
)

(package 
         (name tjr_simple_earley_examples)
         (synopsis "Simple Earley-style parser, examples")
         (depends tjr_simple_earley)
)