$(outdir)/%: %.m4
	$(M4) $< > $@

%.dep:
	touch $@

%.gz: %
	gzip -c9 $< > $@

