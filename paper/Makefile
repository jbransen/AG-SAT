all:
	pdflatex paper.tex
	bibtex paper.aux
	pdflatex paper.tex
	pdflatex paper.tex

lhs2TeX:
	lhs2TeX --poly paper.lhs -o paper.tex

paper.zip:
	zip paper.zip paper.tex loags.tex formats.tex biblio.bib diagrams/*

