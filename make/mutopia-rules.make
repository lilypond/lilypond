

$(outdir)/%.ly.txt: %.ly
	ln -f $< $@

$(outdir)/%.ly.txt: $(outdir)/%.ly
	cp -f $< $@

$(outdir)/%.ly.txt: %.abc
#which file to show here -- abc seems more cute?
	ln -f $< $@

$(outdir)/%.ly: %.abc
	$(PYTHON) $(ABC2LY) --strict -o $@ $< 

$(outdir)/%.png $(outdir)/%.pdf $(outdir)/%.ly $(outdir)/%.ps: $(outdir)/%.ly
# hmm. notdir builds srcdir builds? 
	cd $(outdir); $(LILYPOND) --pdf --ps --png $(notdir $<)

$(outdir)/%.ly: %.ly
	cp $< $@ 


