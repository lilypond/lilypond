
# Don't remove $(outdir)/.log's.  Logs are a target!

$(outdir)/%.0: %.mf $(outdir)/mfplain.mem  
	-$(METAPOST) "&$(outdir)/mfplain \mode=lowres; \mag=1.0; nonstopmode; input $<"

$(outdir)/mfplain.mem: $(MFPLAIN_MP)
	$(INIMETAPOST)  $(INIMETAPOST_FLAGS) $(MFPLAIN_MP) dump
	mv mfplain.* $(outdir)

$(outdir)/%.pfa: $(outdir)/%.0
	$(PYTHON) $(depth)/buildscripts/ps-to-pfa.py --output $(basename $<).pfa $<
	rm -f $(basename $(@F)).[0-9]*
	rm -f $(basename $(@F)).tfm $(basename $(@F)).log

