$(outdir)/%: %
	rm -f $@
	ln $< $@

$(outdir)/%: $(doc-dir)/%.in
	rm -f $@
	cat $< | sed 's%^% %' > $@
