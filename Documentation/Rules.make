# special rules for the documentation section.
# There are too many to add to the general rules

.SUFFIXES: .pod .txt .1  .html

pod2html=pod2html
pod2groff=pod2man --center="LilyPond documentation" --section="0"\
	--release="LilyPond $(VERSION)" $< > $@

$(outdir)/%.gif: %.xpm
	xpmtoppm $< | ppmtogif > $@


$(outdir)/%.ps: $(outdir)/%.dvi
	dvips -o $@ $<

$(outdir)/%.dvi: $(outdir)/%.mudtex
	latex '\nonstopmode \input $<'
	mv $(notdir $@) $(outdir)

$(outdir)/%.mudtex: %.doc
	$(depth)/bin/out/mudela-book --noindex --outdir=$(outdir)/ --outname=$(notdir $@) $<

$(outdir)/%.txt: $(outdir)/%.1
	troff -man -Tascii $< | grotty -b -u -o > $@

$(depth)/%.txt: $(outdir)/%.txt
	cp $< $@

#  perl 5.003/4

POD2HTML5004=$(POD2HTML) --noindex --infile $< --outfile=$@;  sh $(depth)/bin/add-URLs.sh $@

POD2HTML5003=$(POD2HTML) $< ; mv $(notdir $@) $(outdir)/


do_pod2html=$(POD2HTML5003)


$(outdir)/%.html: %.pod $(depth)/VERSION
	$(do_pod2html) 

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
