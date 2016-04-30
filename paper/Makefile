paper.pdf: paper.tex semantics.tex macros.tex
	latexmk -pdf paper.tex

semantics.tex: ../prolog/semantics.eprover ../prolog/tool/LaTeX.hs
	make -C ../prolog/tool/

clean:
	latexmk -C paper.tex

# Experiment:
experiment:
	latexmk -latex='latex %O -shell-escape %S' -pdf paper.tex
