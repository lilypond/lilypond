
$(outdir)/%: %.in $(builddir)/config.make
	rm -f $@
	cat $< | sed $(sed-atfiles) $(sed-atvariables) > $@

