
$(outdir)/%: %.pl
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.bash
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.scm
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.expect
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.sh
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.py
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

