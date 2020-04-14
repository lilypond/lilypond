$(outdir)/%: %.in
	$(call ly_progress,Making,$@,< in)
	rm -f $@
	sed $(sed-atfiles) $(sed-atvariables) < $< > $@

include $(depth)/make/substitute.make
