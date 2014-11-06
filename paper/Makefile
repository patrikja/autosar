paper.pdf: paper.tex
	latexmk -pdf paper.tex

clean:
	latexmk -C paper.tex

# Experiment:
experiment:
	latexmk -latex='latex %O -shell-escape %S' -pdf paper.tex
