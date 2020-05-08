.SUFFIXES: .po .pot .mo

$(outdir)/%.mo: %.po
	$(MSGFMT) -o $@ $<
