

$(outdir)/%.gif: $(outdir)/%.ps
	sh $(buildscripts)/ps-to-gifs.sh $<
	-mv $(name-stem)-page*.gif $(outdir)/
	touch $@

$(outdir)/%.ly.txt: %.ly
	ln -f $< $@

$(outdir)/%.fly.txt: %.fly
	ln -f $< $@



# don't junk intermediate .dvi files.  They're easier to view than
# .ps or .gif
.PRECIOUS: $(outdir)/%.dvi

$(outdir)/%.dvi: %.ly
	sh $(depth)/scripts/ly2dvi.sh -S $(topdir) -o $(outdir)  $< 
	-mv $(basename $<).midi $(outdir)

$(outdir)/%.dvi: %.fly
	sh $(depth)/scripts/ly2dvi.sh -S $(topdir) -o $(outdir)  $< 
	-mv $(basename $<).midi $(outdir)
