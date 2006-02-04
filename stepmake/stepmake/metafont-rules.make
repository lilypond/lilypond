
# Don't remove $(outdir)/.log's.  Logs are a target!

# we want to see botched results as well.
$(outdir)/%.dvi: %.mf
	-MFINPUTS=$(src-dir) $(METAFONT) "\scrollmode; input $<;"
	gftodvi $(basename $<)
	mv $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

# This is not metafont, this is feta-specific
$(outdir)/%.log: %.mf
	MFINPUTS=$(src-dir) $(METAFONT) "\mode:=$(MFMODE); nonstopmode; input $<;"
	mv $(@F) $@
	rm $(basename $(@F)).*gf

$(outdir)/%.tfm $(outdir)%.log: %.mf
	MFINPUTS=$(src-dir) $(METAFONT) "\mode:=$(MFMODE); nonstopmode; input $<;"
# Let's keep this log output, it saves another mf run.
	mv $(basename $(@F)).log $(basename $(@F)).tfm $(outdir)
	rm $(basename $(@F)).*gf

$(outdir)/%.$(XPM_RESOLUTION)gf: %.mf
	MFINPUTS=$(src-dir) $(METAFONT) "\\mode=$(XPM_MODE); \\input $<"
# Let's keep this log output, it saves another mf run.
	mv $(@F) $(basename $(@F)).log $(basename $(@F)).tfm $(outdir)

$(outdir)/%.$(XPM_RESOLUTION)pk: $(outdir)/%.$(XPM_RESOLUTION)gf
	gftopk $< $@


MFTRACE_FORMATS = pfa pfb svg
$(outdir)/%.pfb $(outdir)/%.svg $(outdir)/%.pfa: %.mf
	MFINPUTS=$(src-dir) $(MFTRACE) $(MFTRACE_FLAGS) -I $(src-dir) -I $(outdir)/ --formats=pfa,pfb,svg $(basename $(@F))
#	-mv $(MFTRACE_FORMATS:%=$(basename $(@F).%)) $(outdir)
	-mv $(basename $(@F)).pfa $(outdir)
	-mv $(basename $(@F)).pfb $(outdir)
	-mv $(basename $(@F)).svg $(outdir)

#%.afm:
#	$(SHELL) $(depth)/buildscripts/tfmtoafm.sh $(shell basename $@ .afm)
#	mv $@ $@.in
