
# config_make dep isn't working. Why?  
$(outdir)/%: %.in $(config_make)
	rm -f $@
	cat $< | sed $(sed-atfiles) $(sed-atvariables) > $@


