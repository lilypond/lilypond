# Pod_rules.make

.SUFFIXES: .1 .5 .html .pod .txt

pod2html=pod2html
pod2groff=pod2man --center="$(PACKAGE_NAME) documentation" --section="0"\
	--release="$(PACKAGE_NAME) $(VERSION)" $< > $@


# perl 5.003/4
POD2HTML_5003=$(POD2HTML) $< ; mv $(notdir $@) $(outdir)/ || true
# urg, broken.  if you must have perl 5004, better install pod2html from 5003
POD2HTML_5004=$(POD2HTML) --noindex --infile $< --outfile=$@;  sh $(depth)/bin/add-URLs.sh $@

do_pod2html=$($(POD2HTML_VERSION))

$(outdir)/%.html: $(outdir)/%.pod $(depth)/VERSION
	$(do_pod2html) 

$(outdir)/%.pod: %.pod
	cp $< $@

$(outdir)/%.5: %.pod
	$(pod2groff)

$(outdir)/%.1: %.pod
	$(pod2groff)

$(outdir)/%.1: $(outdir)/%.pod
	-$(pod2groff)

