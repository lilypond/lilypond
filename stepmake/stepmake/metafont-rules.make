
# Don't remove $(outdir)/.log's.  Logs are a target!

# we want to see botched results as well.
$(outdir)/%.dvi: %.mf
	-$(METAFONT) "\nonstopmode; input $<;"
	gftodvi  $(basename $<)
	mv $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

$(outdir)/%.log: %.mf
	$(METAFONT) $<
	mv $(@F) $@
	rm $(basename $(@F)).*gf

$(outdir)/%.tfm $(outdir)%.log: %.mf
	$(METAFONT) "\mode:=$(MFMODE); nonstopmode; input $<;"
# Let's keep this log output, it saves another mf run.
	mv $(basename $(@F)).log $(basename $(@F)).tfm $(outdir)
	rm $(basename $(@F)).*gf 

$(outdir)/%.$(XPM_RESOLUTION)gf: %.mf
	$(METAFONT) "\\mode=$(XPM_MODE); \\input $<"
# Let's keep this log output, it saves another mf run.
	mv $(@F) $(basename $(@F)).log $(basename $(@F)).tfm $(outdir)

$(outdir)/%.$(XPM_RESOLUTION)pk: $(outdir)/%.$(XPM_RESOLUTION)gf
	gftopk $< $@


$(outdir)/%.pfa: %.mf
	pktrace -I $(outdir)/ --pfa --simplify --keep-trying $(basename $(@F))
	mv $(basename $(@F)).pfa $(outdir)

$(outdir)/%.pfb: %.mf
	pktrace -I $(outdir)/ --pfb --simplify --keep-trying  $(basename $(@F))
	mv $(basename $(@F)).pfb $(outdir)

#%.afm:
#	$(SHELL) $(depth)/buildscripts/tfmtoafm.sh $(shell basename $@ .afm)
#	mv $@ $@.in
