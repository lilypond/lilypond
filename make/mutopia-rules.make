$(outdir)/%.ly: %.ly
	$(call ly_progress,Making,$@,(copy))
	cp $< $@

$(outdir)/%.ily: %.ily
	$(call ly_progress,Making,$@,(copy))
	cp $< $@

$(outdir)/%.ly: %.abc
#which file to show here -- abc seems more cute?
	$(call ly_progress,Making,$@,< abc (copy))
	cp $< $@

$(outdir)/%.ly: %.abc
	$(call ly_progress,Making,$@,< abc)
	$(PYTHON) $(ABC2LY) --strict -o $@ $<

# hmm. notdir builds src-dir builds?
$(outdir)/%.png $(outdir)/%.pdf $(outdir)/%.ly $(outdir)/%.ps: $(outdir)/%.ly $(INIT_LY_SOURCES) $(SCHEME_SOURCES)
	$(call ly_progress,Making,$@,< ly)
	cd $(outdir); $(LILYPOND_BINARY) --pdf --png -dlog-file="'$(basename $(notdir $<)).log"   -ddump-signatures -danti-alias-factor=2 -ddelete-intermediate-files -dno-point-and-click -I $(call absdir,$(src-dir))/ $(notdir $<)
	touch $(outdir)/$(basename $(notdir $<)).png

