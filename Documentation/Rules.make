# special rules for the documentation section.
# There are too many to add to the general rules

.SUFFIXES: .pod .txt .1  .html


$(outdir)/%.gif: %.xpm
	xpmtoppm $< | ppmtogif > $@


$(outdir)/%.ps: $(outdir)/%.dvi
	dvips -o $@ $<

$(outdir)/%.dvi: $(outdir)/%.mudtex
	latex '\nonstopmode \input $<'
	mv $(notdir $@) $(outdir)

$(outdir)/%.mudtex: %.doc
	$(binout)/mudela-book --noindex --outdir=$(outdir)/ --outname=$(notdir $@) $<

$(outdir)/%.txt: $(outdir)/%.1
	troff -man -Tascii $< | grotty -b -u -o > $@

$(depth)/%.txt: $(outdir)/%.txt
	cp $< $@

do_pod2html=$(pod2html) --noindex --infile $< --outfile=$@;  sh $(depth)/bin/add-URLs.sh $@
# do this for perl 5.003
# 	do_pod2html=$(pod2html) $<
# 	mv $(notdir $@) $(outdir)/


# do this for perl 5.004
#	 $ make do_pod2html='$(pod2html) --infile $< --outfile=$@' html


$(outdir)/%.html: %.pod $(depth)/VERSION
	$(do_pod2html) 
#

$(outdir)/%.5: %.pod
	$(pod2groff)

$(outdir)/%.1: %.pod
	$(pod2groff)

$(outdir)/%.gz: $(outdir)/%
	gzip -c9 $< > $@

name-stem= $(notdir $(basename $<))

$(outdir)/%.gif: $(outdir)/%.ps
	sh $(depth)/bin/ps-to-gifs.sh $<
	mv $(name-stem)-page*.gif $(outdir)/
	touch $@

$(outdir)/%.ly.txt: $(depth)/input/%.ly
	ln -f $< $@
