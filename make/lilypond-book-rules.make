.SUFFIXES: .html .xml .lytex .tex .latex .lyxml .tely .texi .texinfo


$(outdir)/%.texi:  %.tely
	$(call ly_progress,Making,$@,< tely)
	$(LILYPOND_BOOK_COMMAND) -o $(dir $@) $<

$(outdir)/%.texi:  %.texi
	$(call ly_progress,Making,$@,< texi)
	$(LILYPOND_BOOK_COMMAND) -o $(dir $@) $<
