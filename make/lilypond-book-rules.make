.SUFFIXES: .html .xml .lytex .tex .latex

$(outdir)/%.html:  %.html
	$(LILYPOND_BOOK_COMMAND) -o $(outdir) $<

$(outdir)/%.html:  %.xml
	$(LILYPOND_BOOK_COMMAND) -o $(outdir) $<

$(outdir)/%.tex:  %.lytex
	$(LILYPOND_BOOK_COMMAND) --pdf -o $(outdir) $<

$(outdir)/%.tex:  %.tex
	$(LILYPOND_BOOK_COMMAND) --pdf -o $(outdir) $<

$(outdir)/%.tex:  %.latex
	$(LILYPOND_BOOK_COMMAND) --pdf -o $(outdir) $<

$(outdir)/%.pdf:  $(outdir)/%.tex
	cd $(outdir) && pdflatex $(notdir $<)

$(outdir)/%.xml:  %.lyxml
	$(LILYPOND_BOOK_COMMAND) -o $(outdir) $<
