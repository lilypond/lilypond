
$(outdir)/%: %.in
	rm -f $@
	cat $< | sed $(sed-atfiles) $(sed-atvariables) > $@

