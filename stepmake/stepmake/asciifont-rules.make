$(outdir)/%.afm: %.af
	grep '[[:cntrl:]]' $< | sed 's/^[[:cntrl:]] *//' > $@
#	grep '[[:cntrl:]]' $< | sed 's/^. *//' > $@
