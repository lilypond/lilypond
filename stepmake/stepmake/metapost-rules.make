
# urg
# i've got no idea what this scaling could be for, on both sides...
# it seems that 'low_res', which should be all we ever need according
# to the metapost mfplain guru, really does 200dpi, iso 600dpi (minimun)
#	-$(METAPOST) "&mfmp \mode=ljfour; \mag=100.0; batchmode; input $<"
#	-$(METAPOST) "&mfplain \mode=lowres; \mag=100.0; batchmode; input $<"

$(outdir)/%.0: %.mf mfplain.mem  
	-$(METAPOST) "&mfplain \mode=lowres; \mag=1.0; nonstopmode; input $<"



mfplain.mem: mfplain.ini
	$(INIMETAPOST) mfplain.ini


$(outdir)/%.pfa: $(outdir)/%.0
	$(PYTHON) $(depth)/buildscripts/ps-to-pfa.py --output $(basename $<).pfa $<
	rm -f $(basename $(@F)).[0-9]*
	rm -f $(basename $<).log $(basename $<).tfm



#dokkum:~/usr/src/lilypond/mf$ inimpost /usr/share/texmf/metapost/base/mfplain.mp
#[....] \dump

