
$(outdir)/%: %.pl
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

#FIXME.  Check for bash?
$(outdir)/%: %.sh
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.py
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

