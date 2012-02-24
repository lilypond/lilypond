.SUFFIXES: .html .xml .lytex .tex .latex .lyxml .tely .texi .texinfo

############## HTML #########################

$(outdir)/%.html:  %.html
	$(LILYPOND_BOOK_COMMAND) -o $(outdir) $<

$(outdir)/%.html:  %.htmly
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

# Add the tex => pdf rule only if we have pdflatex
ifeq (,$(findstring pdflatex,$(MISSING_OPTIONAL)))
$(outdir)/%.pdf:  $(outdir)/%.tex
	cd $(outdir) && $(PDFLATEX) $(notdir $<)
endif

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

# Add the xml => pdf rule only if we have dblatex
ifeq (,$(findstring dblatex,$(MISSING_OPTIONAL)))
$(outdir)/%.pdf:  $(outdir)/%.xml
	cd $(outdir) && $(DBLATEX) $(notdir $<)
endif
