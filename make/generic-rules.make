# title	   package specific rules
# file	   make/Rules.make

# urg
$(outdir)/%.ly: %.lym4
	$(M4) $< | sed "s/\`/,/g" > $@

$(outdir)/%: %.in
	rm -f $@
	cat $< | sed $(sed-atfiles) $(sed-atvariables) > $@



include $(depth)/make/substitute.make



