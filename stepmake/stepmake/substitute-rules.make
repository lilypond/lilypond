
# config_make dep isn't working. Why?
$(outdir)/%: %.in $(config_make) $(depth)/VERSION
	rm -f $@
	cat $< | sed $(sed-atfiles) | sed $(sed-atvariables) > $@


