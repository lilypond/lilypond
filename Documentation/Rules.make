# special rules for the documentation section.
# There are too many to add to the general rules

.SUFFIXES: .pod .text .1  .html


$(outdir)/%.xpm: %.gif
	giftopnm $< | ppmtoxpm > $@

$(outdir)/%.ps: $(outdir)/%.dvi
	dvips -o $@ $<

$(outdir)/%.dvi: $(outdir)/%.mudtex
	latex '\nonstopmode \input $<'
	mv $(notdir $@) $(outdir)

$(outdir)/%.mudtex: %.doc
	$(binout)/mudela-book --outdir=$(outdir)/ --outname=$(notdir $@) $<

$(outdir)/%.text: $(outdir)/%.1
	groff -man -Tascii $< > $@

$(depth)/%.text: $(outdir)/%.text
	cp $< $@

do_pod2html=$(pod2html) $<

# do this for perl 5.004
#	 $ make do_pod2html='$(pod2html) --infile $< --outfile=$@' html
#
$(outdir)/%.html: %.pod $(depth)/VERSION
	$(do_pod2html) 
	mv $(notdir $@) $(outdir)/

$(outdir)/%.5: %.pod
	$(pod2groff)

$(outdir)/%.1: %.pod
	$(pod2groff)

$(outdir)/%.gz: $(outdir)/%
	gzip -c9 $< > $@

name-stem= $(notdir $(basename $<))
$(outdir)/%.dvi: $(depth)/input/%.ly 
	(cd $(outdir); \
	rm lelie.midi ; \
	lilypond -o  $(name-stem)  ../$< )
	(cd $(outdir); \
	if [ -f ../$(basename $< ).tex ]; \
	then \
		latex ../$(basename $< ) ;\
	else \
		tex $(name-stem) ;\
	fi)


# generate the pixmap at twice the size, then rescale (for antialiasing)
$(outdir)/%.gif: $(outdir)/%.ps
	gs -q -sDEVICE=ppmraw -sOutputFile=- -r200 -dNOPAUSE  $< -c quit |pnmscale 0.5| ppmtogif > $@

$(outdir)/%.ly.txt: $(depth)/input/%.ly
	ln -f $< $@
