
$(outdir)/%: %.in
	rm -f $@
	cat $< | $(sed-atfiles) | $(sed-atvariables) > $@

