

$(outdir)/%.gif: $(outdir)/%.ps
	sh $(buildscripts)/ps-to-gifs.sh $<
	-mv $(name-stem)-page*.gif $(outdir)/
	touch $@

$(outdir)/%.png: $(outdir)/%.ps
	sh $(buildscripts)/ps-to-pngs.sh $<
	-mv $(name-stem)-page*.png $(outdir)/
	touch $@

$(outdir)/%.ly.txt: %.ly
	ln -f $< $@

$(outdir)/%.fly.txt: %.fly
	ln -f $< $@



# don't junk intermediate .dvi files.  They're easier to view than
# .ps or .gif
.PRECIOUS: $(outdir)/%.dvi

$(outdir)/%.dvi: %.ly
	$(PYTHON) $(depth)/scripts/ly2dvi.py -o $(outdir)  $< 
	-mv $(basename $<).midi $(outdir)

$(outdir)/%.dvi: %.fly
	$(PYTHON) $(depth)/scripts/ly2dvi.py -o $(outdir)  $< 
	-mv $(basename $<).midi $(outdir)
