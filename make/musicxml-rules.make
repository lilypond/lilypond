.SUFFIXES: .xml .mxl


$(outdir)/%.ly:  %.xml
	$(call ly_progress,Making,$@,< xml)
	$(PYTHON) $(MUSICXML2LY) -o $@ $<

$(outdir)/%.ly:  %.mxl
	$(call ly_progress,Making,$@,< mxl)
	$(PYTHON) $(MUSICXML2LY) -z -o $@ $<
