

$(outdir)/%.ly.txt: %.ly
	ln -f $< $@

$(outdir)/%.ly.txt: $(outdir)/%.ly
	cp -f $< $@

$(outdir)/%.ly.txt: %.abc
#which file to show here -- abc seems more cute?
	ln -f $< $@

$(outdir)/%.ly: %.abc
	$(PYTHON) $(ABC2LY) --strict -o $@ $< 

# hmm. notdir builds srcdir builds? 
$(outdir)/%.png $(outdir)/%.pdf $(outdir)/%.ly $(outdir)/%.ps: $(outdir)/%.ly
	cd $(outdir); $(LILYPOND) --pdf --png -ddelete-intermediate-files -dno-point-and-click -I $(shell pwd)/ $(notdir $<)
	touch $(outdir)/$(basename $(notdir $<)).png

$(outdir)/%.ly: %.ly
	cp $< $@ 


