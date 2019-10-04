# title	   package specific rules
# file	   make/Rules.make

# urg
$(outdir)/%.ly: %.lym4
	$(call ly_progress,Making,$@,< lym4)
	$(M4) $< | sed "s/\`/,/g" > $@

$(outdir)/%: %.in
	$(call ly_progress,Making,$@,< in)
	rm -f $@
	cat $< | sed $(sed-atfiles) | sed $(sed-atvariables) > $@



include $(depth)/make/substitute.make



