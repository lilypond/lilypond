
$(outdir)/%.dvi: %.mf
	mf $<
	gftodvi  $(basename $<)
	mv   $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

$(outdir)/%.log: %.mf
	mf $<
	mv $(@F) $@
	rm $(basename $< ).*gf

$(outdir)/%.tfm: %.mf
	mf '\mode:=ljfour;  input $<;'
	mv $(@F) $(outdir)
	rm $(basename $<).*gf $(basename $<).*log

$(outdir)/%.$(XPM_RESOLUTION)gf: %.mf
	mf "\\mode=$(XPM_MODE); \\input $<"
	mv $(@F) out
	rm -f $(basename $<).log $(basename $<).tfm

$(outdir)/%.$(XPM_RESOLUTION)pk: $(outdir)/%.$(XPM_RESOLUTION)gf
	gftopk $< $@

