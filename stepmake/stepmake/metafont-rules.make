
# Don't remove $(outdir)/.log's.  Logs are a target!

# we want to see botched results as well.
$(outdir)/%.dvi: %.mf
	-MFINPUTS=$(src-dir) $(METAFONT) "\scrollmode; input $<;"
	gftodvi $(basename $<)
	mv $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

$(outdir)/%.tfm $(outdir)%.log: %.mf
	MFINPUTS=$(src-dir) $(METAFONT) "\mode:=$(MFMODE); nonstopmode; input $<;"
# Let's keep this log output, it saves another mf run.
	mv $(basename $(@F)).log $(basename $(@F)).tfm $(outdir)
	rm -f $(basename $(@F)).*gf  $(basename $(@F)).*pk


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
