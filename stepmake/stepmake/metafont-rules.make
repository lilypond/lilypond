
$(outdir)/%.dvi: %.mf
	$(METAFONT) "\nonstopmode; input $<;"
	gftodvi  $(basename $<)
	mv   $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

$(outdir)/%.log: %.mf
	$(METAFONT) $<
	mv $(@F) $@
	rm $(basename $< ).*gf

$(outdir)/%.tfm: %.mf
	$(METAFONT) "\mode:=$(MFMODE); nonstopmode; input $<;"
	mv $(@F) $(outdir)
	rm $(basename $<).*gf $(basename $<).*log

$(outdir)/%.$(XPM_RESOLUTION)gf: %.mf
	$(METAFONT) "\\mode=$(XPM_MODE); \\input $<"
	mv $(@F) out
	rm -f $(basename $<).log $(basename $<).tfm

$(outdir)/%.$(XPM_RESOLUTION)pk: $(outdir)/%.$(XPM_RESOLUTION)gf
	gftopk $< $@

#%.afm:
#	$(SHELL) $(depth)/buildscripts/tfmtoafm.sh $(shell basename $@ .afm)
#	mv $@ $@.in
