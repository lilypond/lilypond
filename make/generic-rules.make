# title	   package specific rules
# file	   make/Rules.make

# urg
$(outdir)/%.ly: %.lym4
	$(M4) $< | sed "s/\`/,/g" > $@

$(outdir)/%: %.in
	rm -f $@
	cat $< | sed $(sed-atfiles) $(sed-atvariables) > $@



include $(depth)/make/substitute.make


# HUH???
$(outdir)/%.hh: $(doc-dir)/%.in
	rm -f $@
	echo '_(' > $@
	cat $< | $(sed-quotes) | $(sed-newline) | $(sed-quote-line) >> $@
	echo ');' >> $@

