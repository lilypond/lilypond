# title	   package specific rules
# file	   make/Rules.make

# urg
$(outdir)/%.ly: %.lym4
	$(M4) $< | sed "s/\`/,/g" > $@

$(outdir)/%: %.in
	rm -f $@
	cat $< | $(sed-atfiles) | $(sed-atvariables) > $@



include $(depth)/make/Substitute.make

# $(depth)/make/$(outdir)/%.make: $(depth)/make/%.in
# 	rm -f $@
#	echo "$(basename $(@F)) = \\" > $@
#	cat $< | $(sed-newline) | $(sed-endline) | $(sed-quotes) >> $@
#	echo >> $@
#	echo >> $@

# HUH???
$(outdir)/%.hh: $(doc-dir)/%.in
	rm -f $@
	echo '_(' > $@
	cat $< | $(sed-quotes) | $(sed-newline) | $(sed-quote-line) >> $@
	echo ');' >> $@

