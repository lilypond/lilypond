# Texinfo_rules.make

.SUFFIXES: .html .info .texi .texinfo

$(outdir)/%.info: $(outdir)/%.texi
	-$(MAKEINFO) --force --output=$@ $<

$(outdir)/%.html:	$(outdir)/%.texi
	-$(MAKEINFO) --force --output=$@ --html --no-headers $< 
# we want footers even if website builds (or is built) partly
	$(footify) $@

$(outdir)/%.dvi:	$(outdir)/%.texi
# --clean only in >= 3.12s
# cd $(outdir); texi2dvi --clean ../$< 
	cd $(outdir); texi2dvi ../$< 

$(outdir)/%.txt: $(outdir)/%.texi
	$(MAKEINFO) -I../  --no-split --no-headers --output $@ $<

$(outdir)/%.texi: %.texi
	cp $< $@



