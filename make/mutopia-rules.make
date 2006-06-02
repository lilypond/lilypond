

$(outdir)/%.ly: %.ly
	cp $< $@

$(outdir)/%.ly: %.abc
	$(PYTHON) $(ABC2LY) --strict -o $@ $<

# hmm. notdir builds src-dir builds?
$(outdir)/%.png $(outdir)/%.pdf $(outdir)/%.ly $(outdir)/%.ps: $(outdir)/%.ly
	cd $(outdir); $(LILYPOND_BINARY) --pdf --png -ddump-signatures -danti-alias-factor=2 -ddump-signatures -ddelete-intermediate-files -dno-point-and-click -I $(call absdir,$(src-dir))/ $(notdir $<)
	touch $(outdir)/$(basename $(notdir $<)).png


