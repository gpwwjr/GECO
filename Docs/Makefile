
.phony: clean

all: geco.pdf

geco.pdf: $(wildcard *.sty) $(wildcard *.tex)
	-pdflatex --interaction=nonstopmode geco.tex
	makeindex geco.idx
	bibtex geco
	pdflatex geco.tex
	pdflatex geco.tex

clean:
	-rm *.pdf *.aux *.log *.toc *.bbl *.blg *.idx *.ilg *.ind *.lof *.xml

