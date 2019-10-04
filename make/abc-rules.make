.SUFFIXES: .abc

$(outdir)/%.ly:  %.abc
	$(call ly_progress,Making,$@,< abc)
	$(PYTHON) $(ABC2LY) --quiet -o $@ $<
