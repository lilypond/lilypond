
$(outdir)/%: %.pl $(config_make)
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.bash $(config_make)
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.scm $(config_make)
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.expect $(config_make)
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.sh $(config_make)
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.py $(config_make)
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

