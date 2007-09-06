.SUFFIXES: .xml


$(outdir)/%.ly:  %.xml
	$(PYTHON) $(MUSICXML2LY) -o $@ $<
