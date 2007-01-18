$(outdir)/%: %.m4
	$(M4) $< > $@

%.gz: %
	gzip -c9 $< > $@

