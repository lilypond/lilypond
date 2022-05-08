.SUFFIXES: .po .pot .mo

$(outdir)/%/LC_MESSAGES/lilypond.mo: %.po
	mkdir -p $(dir $@)
	$(MSGFMT) -o $@ $<
