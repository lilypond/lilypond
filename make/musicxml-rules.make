.SUFFIXES: .xml .mxl


$(outdir)/%.ly:  %.xml
	$(PYTHON) $(MUSICXML2LY) -o $@ $<

$(outdir)/%.ly:  %.mxl
	$(PYTHON) $(MUSICXML2LY) -z -o $@ $<
