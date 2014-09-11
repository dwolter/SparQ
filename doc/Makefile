FILE = SparQ-Manual



default: pdf

pdf: $(FILE).pdf

publish: pdf

all:

$(FILE).pdf:
	pdflatex $(FILE)
	bibtex $(FILE)
	pdflatex $(FILE)
	pdflatex $(FILE)


pdf:
	pdflatex $(FILE)
	bibtex $(FILE)
	pdflatex $(FILE)
	pdflatex $(FILE)



quick:
	pdflatex $(FILE)
	cp $(FILE).pdf $(FILE)_view.pdf

clean:
	rm -f *.aux *.log *.blg *.bbl *.toc
	rm -f *.4tc *.4ct *.tmp *.lg *.idv *.xref
	rm -f $(FILE).pdf $(FILE).ps $(FILE).dvi $(FILE).css $(FILE).html
	rm -f $(FILE)/*

distclean: clean

dist:

distdir: 

html:
	ps
	htlatex $(FILE) "html,uni-html4,charset=utf-8"
	mv *.html *.css ri-engine-manual
