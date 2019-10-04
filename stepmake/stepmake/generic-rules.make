$(outdir)/%: %.m4
	$(call ly_progress,Making,$@,< m4)
	$(M4) $< > $@

%.gz: %
	$(call ly_progress,Making,$@,)
	gzip -c9 $< > $@
