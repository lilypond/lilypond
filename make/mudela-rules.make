# Mudela_rules.make

.SUFFIXES: .doc .dvi .mudtex



$(outdir)/%.latex: %.doc
	$(PYTHON) $(depth)/scripts/mudela-book.py -I $(depth)/input/test/ --outdir=$(outdir)/ --dependencies --outname=$(notdir $(basename $@)) $<

