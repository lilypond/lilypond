# Mudela_rules.make

.SUFFIXES: .doc .dvi .mudtex



$(outdir)/%.latex: %.doc
	$(PYTHON) $(depth)/scripts/mudela-book.py --outdir=$(outdir)/ --outname=$(notdir $(basename $@)) $<

