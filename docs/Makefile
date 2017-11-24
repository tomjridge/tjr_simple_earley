all:
	asciidoctor tjr_earley.adoc

# NOTE --latex-engine will soon become --pdf-engine
#    --latex-engine=lualatex 
tmp.tex: FORCE
	./mk_latex.ml >tmp.md_with_latex
	pandoc --from=markdown+raw_tex --to=latex \
    --template=pandoc_latex_template.modified \
    --metadata=title:'Simple Earley Parsing' \
    --metadata=author:'Tom Ridge' \
    --metadata=date:'2017-11-24' \
    --number-sections \
	  --standalone tmp.md_with_latex > tmp.tex
	lualatex tmp.tex

clean:
	rm -f tmp.*

FORCE:
