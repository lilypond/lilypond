

$(outdir)/%.gif: $(outdir)/%.ps
	sh $(PS_TO_GIFS) $<
	-mv $(name-stem)-page*.gif $(outdir)/
	touch $@

$(outdir)/%.png: $(outdir)/%.ps
	sh $(PS_TO_PNGS) $<
	-mv $(name-stem)-page*.png $(outdir)/
	touch $@

$(outdir)/%.ly.txt: %.ly
	ln -f $< $@

$(outdir)/%.ly.txt: %.abc
#which file to show here -- abc seems more cute?
	ln -f $< $@

$(outdir)/%.ly: %.abc
	$(PYTHON) $(ABC2LY) -o $@ $< 

$(outdir)/%.dvi: $(outdir)/%.ly
	$(PYTHON) $(LY2DVI) --outdir=$(outdir) --dependencies $< 

# don't junk intermediate .dvi files.  They're easier to view than
# .ps or .png
.PRECIOUS: $(outdir)/%.dvi

$(outdir)/%.dvi: %.ly
	$(PYTHON) $(LY2DVI) --outdir=$(outdir) --dependencies $< 

$(outdir)-$(PAPERSIZE)/%.dvi: %.ly
	$(PYTHON) $(LY2DVI) --outdir=$(outdir)-$(PAPERSIZE) --dependencies --set=papersize=$(PAPERSIZE) $< 

