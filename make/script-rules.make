
$(outdir)/%: %.pl $(config_make) $(depth)/VERSION
	$(call ly_progress,Making,$@,(sed))
	sed $(sed-atfiles) $(sed-atvariables) < $< > $@
	chmod 755 $@

$(outdir)/%: %.bash $(config_make) $(depth)/VERSION
	$(call ly_progress,Making,$@,(sed))
	sed $(sed-atfiles) $(sed-atvariables) < $< > $@
	chmod 755 $@

$(outdir)/%: %.scm $(config_make) $(depth)/VERSION
	$(call ly_progress,Making,$@,(sed))
	sed $(sed-atfiles) $(sed-atvariables) < $< > $@
	chmod 755 $@

$(outdir)/%: %.sh $(config_make) $(depth)/VERSION
	$(call ly_progress,Making,$@,(sed))
	sed $(sed-atfiles) $(sed-atvariables) < $< > $@
	chmod 755 $@

$(outdir)/%: %.py $(config_make) $(depth)/VERSION
	$(call ly_progress,Making,$@,(sed))
	sed $(sed-atfiles) $(sed-atvariables) < $< > $@
	chmod 755 $@
