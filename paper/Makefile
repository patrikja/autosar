paper.pdf: paper.tex semantics.tex macros.tex refs.bib
	latexmk -pdf paper.tex

semantics.tex: ../sem/semantics.eprover ../sem/tool/LaTeX.hs
	make -C ../sem/tool/

clean:
	latexmk -C paper.tex

# Experiment:
experiment:
	latexmk -latex='latex %O -shell-escape %S' -pdf paper.tex
