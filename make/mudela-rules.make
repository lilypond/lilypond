# Mudela_rules.make

.SUFFIXES: .doc .dvi .mudtex

$(outdir)/%.dvi: $(outdir)/%.mudtex
	latex '\nonstopmode \input $<'
	mv $(notdir $@) $(outdir)

$(outdir)/%.mudtex: %.doc
	$(PYTHON) $(depth)/scripts/mudela-book.py --outdir=$(outdir)/ --outname=$(notdir $@) $<

