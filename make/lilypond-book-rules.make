.SUFFIXES: .html .xml .lytex .tex .latex .lyxml .tely .texi .texinfo

############## HTML #########################

$(outdir)/%.html:  %.html
	$(LILYPOND_BOOK_COMMAND) -o $(outdir) $<

$(outdir)/%.html:  %.xml
	$(LILYPOND_BOOK_COMMAND) -o $(outdir) $<


############## LaTeX ########################

$(outdir)/%.tex:  %.lytex
	$(LILYPOND_BOOK_COMMAND) --pdf -o $(outdir) $<

$(outdir)/%.tex:  %.tex
	$(LILYPOND_BOOK_COMMAND) --pdf -o $(outdir) $<

$(outdir)/%.tex:  %.latex
	$(LILYPOND_BOOK_COMMAND) --pdf -o $(outdir) $<

$(outdir)/%.pdf:  $(outdir)/%.tex
	cd $(outdir) && pdflatex $(notdir $<)


############## Texinfo ######################

$(outdir)/%.texi:  %.texi
	$(LILYPOND_BOOK_COMMAND) -o $(outdir) $<

$(outdir)/%.texi:  %.itexi
	$(LILYPOND_BOOK_COMMAND) -o $(outdir) $<

$(outdir)/%.texi:  %.texinfo
	$(LILYPOND_BOOK_COMMAND) -o $(outdir) $<

$(outdir)/%.texi:  %.tely
	$(LILYPOND_BOOK_COMMAND) -o $(outdir) $<


############## DocBook ######################

$(outdir)/%.xml:  %.lyxml
	$(LILYPOND_BOOK_COMMAND) --pdf -o $(outdir) $<

$(outdir)/%.pdf:  $(outdir)/%.xml
	cd $(outdir) && dblatex $(notdir $<)
