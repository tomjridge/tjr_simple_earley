all:
	asciidoctor tjr_earley.adoc

# NOTE --latex-engine will soon become --pdf-engine
#    --latex-engine=lualatex 
tmp.pdf: mk_latex.native tjr_earley.md FORCE
	./mk_latex.native >tmp.md_with_latex
	pandoc --from=markdown+raw_tex --to=latex \
    --template=pandoc_latex_template.modified \
    --metadata=title:'Simple Earley Parsing' \
    --metadata=author:'Tom Ridge' \
    --metadata=date:'2017-11-24' \
    --number-sections \
	  --standalone tmp.md_with_latex > tmp.tex
	lualatex tmp.tex

mk_latex.native: mk_latex.ml
	ocamlfind ocamlopt -package omacro -linkpkg -thread -o mk_latex.native mk_latex.ml

clean:
	rm -f tmp.*

FORCE:
