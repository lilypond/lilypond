
# config_make dep isn't working. Why?
$(outdir)/%: %.in $(config_make) $(depth)/VERSION
	$(call ly_progress,Making,$@,< in)
	rm -f $@
	sed $(sed-atfiles) $(sed-atvariables) < $< > $@
