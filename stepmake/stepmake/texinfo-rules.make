# Texinfo_rules.make

.SUFFIXES: .html .info .texi .texinfo

$(outdir)/%.info: $(outdir)/%.texi
	makeinfo --output=$@ $<

$(outdir)/%.html:	$(outdir)/%.texi
	-makeinfo --force --output=$@ --html --no-headers $< 

$(outdir)/%.dvi:	$(outdir)/%.texi
# --clean only in >= 3.12s
# cd $(outdir); texi2dvi --clean ../$< 
	cd $(outdir); texi2dvi ../$< 

$(outdir)/%.txt: $(outdir)/%.texi
	$(MAKEINFO) -I../  --no-split --no-headers --output $@ $<

$(outdir)/%.texi: %.texi
	cp $< $@

