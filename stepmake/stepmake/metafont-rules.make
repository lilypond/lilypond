# Don't remove $(outdir)/.log's.  Logs are a target!

# we want to see botched results as well.
$(outdir)/%.dvi: %.mf
	-MFINPUTS=$(src-dir) $(METAFONT) "\scrollmode; input $<;"
	gftodvi $(basename $<)
	mv $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

$(outdir)/%.tfm $(outdir)/%.log: %.mf
	MFINPUTS=$(src-dir) $(METAFONT) "\mode:=$(MFMODE); nonstopmode; input $<;"
# Let's keep this log output, it saves another mf run.
	mv $(basename $(@F)).log $(basename $(@F)).tfm $(outdir)
	rm -f $(basename $(@F)).*gf  $(basename $(@F)).*pk

$(outdir)/%.pfb: %.mf $(outdir)/mf2pt1.mem
	cd $(outdir) && MFINPUTS=.. $(PERL) ../$(buildscript-dir)/mf2pt1.pl $(MF2PT1_OPTIONS) $<

$(outdir)/mf2pt1.mem:
	cd $(outdir) && mpost -progname=mpost -ini ../mf2pt1 \\dump
