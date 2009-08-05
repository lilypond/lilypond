$(outdir)/%: %.m4
	$(M4) $< > $@

%.gz: %
	gzip -c9 $< > $@

$(outdir)/%.css: $(CSS_DIRECTORY)/%.css
	ln -f $< $@
