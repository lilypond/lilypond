# Scripts.make




$(outdir)/%: %.pl
	cat $< | $(sed-atvariables) > $@
	chmod 755 $@

#FIXME.  Check for bash?
$(outdir)/%: %.sh
	cat $< | $(sed-atvariables) > $@
	chmod 755 $@


$(outdir)/%: %.py
	cat $< | $(sed-atvariables) > $@
	chmod 755 $@

