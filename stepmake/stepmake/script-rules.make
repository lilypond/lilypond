
$(outdir)/%: %.pl $(config_make)  $(depth)/VERSION
	cat $< | sed $(sed-atfiles) | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.bash $(config_make) $(depth)/VERSION
	cat $< | sed $(sed-atfiles) | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.scm $(config_make) $(depth)/VERSION
	cat $< | sed $(sed-atfiles) | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.expect $(config_make) $(depth)/VERSION
	cat $< | sed $(sed-atfiles) | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.sh $(config_make) $(depth)/VERSION
	cat $< | sed $(sed-atfiles) | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.py $(config_make) $(depth)/VERSION
	cat $< | sed $(sed-atfiles) | sed $(sed-atvariables) > $@
	chmod 755 $@

