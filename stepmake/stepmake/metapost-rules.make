
$(outdir)/%.pfa: %.mf
# urg
# i've got no idea what this scaling could be for, on both sides...
# it seems that 'low_res', which should be all we ever need according
# to the metapost mfplain guru, really does 200dpi, iso 600dpi (minimun)
	$(METAPOST) "&mfplain \mode=lowres; \mag=100.0; batchmode; input $<"
#	-$(METAPOST) "&mfmp \mode=ljfour; \mag=100.0; batchmode; input $<"
	$(PYTHON) $(depth)/buildscripts/ps-to-pfa.py $<
	rm -f $(basename $(@F)).[0-9]*
	rm -f $(basename $<).log $(basename $<).tfm

