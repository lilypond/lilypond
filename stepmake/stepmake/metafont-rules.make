# Don't remove $(outdir)/.log's.  Logs are a target!

# we want to see botched results as well.
$(outdir)/%.dvi: %.mf
	$(call ly_progress,Making,$@,< mf)
	-$(DO_MF_DEP) \
	  MFINPUTS=$(src-dir) \
	  max_print_line=1000 \
	  $(METAFONT) "\scrollmode; input $<;" $(METAFONT_QUIET)
	gftodvi $(basename $<)
	mv $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

$(outdir)/%.tfm $(outdir)/%.log: %.mf
	$(call ly_progress,Making,$@,< mf)
	$(DO_MF_DEP) \
	  MFINPUTS=$(src-dir) \
	  max_print_line=1000 \
	  $(METAFONT) "\mode:=$(MFMODE); nonstopmode; input $<;" $(METAFONT_QUIET)
# Let's keep this log output, it saves another mf run.
	mv $(basename $(@F)).log $(basename $(@F)).tfm $(outdir)
	rm -f $(basename $(@F)).*gf  $(basename $(@F)).*pk

# ugh . mf2pt1 is extremely broken, it pollutes CWD iso. creating a
# temp dir.
#
# the soft link for mf2pt1.mp is for recent mpost versions
# which no longer dump a .mem file
$(outdir)/%.pfb: %.mf $(outdir)/mf2pt1.mem $(outdir)/%.log
	$(call ly_progress,Making,$@,< mf)
	$(DO_MF_DEP) TMP_DIR=`mktemp -d $(outdir)/pfbtemp.$*.XXXXXXXXX` \
	&& ( cd $$TMP_DIR \
		&& ln -s ../mf2pt1.mem . \
		&& ln -s ../../mf2pt1.mp . \
		&& MFINPUTS=$(abs-src-dir):..:: \
		   FONTFORGE=$(FONTFORGE) \
		   max_print_line=1000 \
		   $(buildscript-dir)/mf2pt1 $(MF2PT1_OPTIONS) $< $(METAFONT_QUIET)) \
	&& mv $$TMP_DIR/*pfb $(outdir); \
	rm -rf $$TMP_DIR

# since recent mpost versions no longer create a mem file, we create a dummy
# file to satisfy the dependency (which gets overwritten in case an older
# mpost creates a real mem file)
$(outdir)/mf2pt1.mem: mf2pt1.mp
	$(call ly_progress,Making,$@,< mp)
	cd $(outdir) \
	   && touch mf2pt1.mem \
	   && mpost -progname=mpost -ini $(top-src-dir)/mf/mf2pt1.mp \\dump $(METAFONT_QUIET)
