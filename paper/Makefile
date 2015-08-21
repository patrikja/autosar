paper.pdf: paper.tex semantics.tex
	latexmk -pdf paper.tex

clean:
	latexmk -C paper.tex

# Experiment:
experiment:
	latexmk -latex='latex %O -shell-escape %S' -pdf paper.tex
