# special rules for the documentation section.
# There are too many to add to the general rules

.SUFFIXES: .pod .1 $(DOTTEXT) .html

pod2html=pod2html
pod2groff=pod2man --center="LilyPond documentation" --section="0"\
	--release="LilyPond $(VERSION)" $< > $@

$(outdir)/%.gif: %.xpm
	xpmtoppm $< | ppmtogif > $@

$(outdir)/%.dvi: $(outdir)/%.mudtex
	latex '\nonstopmode \input $<'
	mv $(notdir $@) $(outdir)

$(outdir)/%.mudtex: %.doc
	$(depth)/bin/out/mudela-book  --outdir=$(outdir)/ --outname=$(notdir $@) $<

$(outdir)/%$(DOTTEXT): $(outdir)/%.1
	troff -man -Tascii $< | grotty -b -u -o > $@

#$(depth)/%$(DOTTEXT): $(outdir)/%$(DOTTEXT)
# urg
# $(depth)/%$(DOTTEXT): out/%$(DOTTEXT)
#	cp $< $@
# huh?
$(outdir)/%$(DOTTEXT): $(depth)/%
	cp $< $@

#  perl 5.003/4
POD2HTML5004=$(POD2HTML) --noindex --infile $< --outfile=$@;  sh $(depth)/bin/add-URLs.sh $@

POD2HTML5003=$(POD2HTML) $< ; mv $(notdir $@) $(outdir)/


do_pod2html=$(POD2HTML5004)


$(outdir)/%.html: %.pod $(depth)/VERSION
	$(do_pod2html) 
	add-html-footer $@

$(outdir)/%.5: %.pod
	$(pod2groff)

$(outdir)/%.1: %.pod
	$(pod2groff)

