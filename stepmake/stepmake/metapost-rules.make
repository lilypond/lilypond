
$(outdir)/%.0: %.mf mfplain.mem  
	-$(METAPOST) "&mfplain \mode=lowres; \mag=1.0; nonstopmode; input $<"

mfplain.mem: $(MFPLAIN_MP)
	$(INIMETAPOST)  $(INIMETAPOST_FLAGS) $(MFPLAIN_MP) dump


$(outdir)/%.pfa: $(outdir)/%.0
	$(PYTHON) $(depth)/buildscripts/ps-to-pfa.py --output $(basename $<).pfa $<
	rm -f $(basename $(@F)).[0-9]*
	rm -f $(basename $<).log

