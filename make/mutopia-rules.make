

$(outdir)/%.png: $(outdir)/%.ps
	gs -sDEVICE=pnggray -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -sOutputFile="$(name-stem)-page%d.png" -r90 -dNOPAUSE $< -c quit
	-mv $(name-stem)-page*.png $(outdir)/
	rm -f $@
	ln -s $(name-stem)-page1.png $@

$(outdir)/%.ly.txt: %.ly
	ln -f $< $@

$(outdir)/%.ly.txt: $(outdir)/%.ly
	cp -f $< $@

$(outdir)/%.ly.txt: %.abc
#which file to show here -- abc seems more cute?
	ln -f $< $@

$(outdir)/%.ly: %.abc
	$(PYTHON) $(ABC2LY) --strict -o $@ $< 

$(outdir)/%.ps: $(outdir)/%.ly
	$(PYTHON) $(LY2DVI) --output=$@ $<  

$(outdir)/%.ps: %.ly
	$(PYTHON) $(LY2DVI) --output=$@ $<  

$(outdir)/%.pdf: $(outdir)/%.ps
	ps2pdf -sPAPERSIZE=a4 $< $@


