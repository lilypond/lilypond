# Texinfo_rules.make

.SUFFIXES: .html .info .texi .texinfo

$(outdir)/%.info: $(outdir)/%.texi
	makeinfo --output=$@ $<

$(outdir)/%.html:	$(outdir)/%.texi
	-makeinfo --force --output=$@ --html --no-headers $< 
	$(footify) $@

$(outdir)/%.dvi:	$(outdir)/%.texi
# --clean only in >= 3.12s
# cd $(outdir); texi2dvi --clean ../$< 
	cd $(outdir); texi2dvi ../$< 

$(outdir)/%.txt: $(outdir)/%.texi
	$(MAKEINFO) --no-split --no-headers --output $@ $<

$(outdir)/%.texi: %.texi
	cp $< $@

