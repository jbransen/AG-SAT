all:
	pdflatex paper.tex
	bibtex paper.aux
	pdflatex paper.tex
	pdflatex paper.tex

pdf: lhs2TeX
	pdflatex paper.tex

lhs2TeX:
	lhs2TeX --poly paper.lhs -o paper.tex


