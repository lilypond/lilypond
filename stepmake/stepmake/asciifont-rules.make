# WHAT THE FUCK IS THIS DOING IN STEPMAKE???

$(outdir)/%.afm: %.af
	grep '[[:cntrl:]]' $< | sed 's/^[[:cntrl:]] *//' > $@
#	grep '[[:cntrl:]]' $< | sed 's/^. *//' > $@
