

$(outdir)/%.gif: $(outdir)/%.ps
	sh $(buildscript-dir)/ps-to-gifs.sh $<
	-mv $(name-stem)-page*.gif $(outdir)/
	touch $@

$(outdir)/%.png: $(outdir)/%.ps
	sh $(buildscript-dir)/ps-to-pngs.sh $<
	-mv $(name-stem)-page*.png $(outdir)/
	touch $@

$(outdir)/%.ly.txt: %.ly
	ln -f $< $@

$(outdir)/%.ly.txt: %.abc
#which file to show here -- abc seems more cute?
	ln -f $< $@

$(outdir)/%.ly: %.abc
	$(PYTHON) $(script-dir)/abc2ly.py -o $@ $< 

$(outdir)/%.dvi: $(outdir)/%.ly
	$(PYTHON) $(script-dir)/ly2dvi.py -o $(outdir)  $< 
	-mv $(basename $(<F))*.midi $(outdir)

# don't junk intermediate .dvi files.  They're easier to view than
# .ps or .gif
.PRECIOUS: $(outdir)/%.dvi

$(outdir)/%.dvi: %.ly
	$(PYTHON) $(script-dir)/ly2dvi.py -o $(outdir)  $< 
	-mv $(basename $<)*.midi $(outdir)

$(outdir)/%.dvi: %.fly
	$(PYTHON) $(script-dir)/ly2dvi.py -o $(outdir)  $< 
	-mv $(basename $<)*.midi $(outdir)
