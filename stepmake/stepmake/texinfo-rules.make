# Texinfo_rules.make

.SUFFIXES: .html .info .texi .texinfo

$(outdir)/%.info: $(outdir)/%.texi
	-$(MAKEINFO) --force --output=$@ $<

$(outdir)/%.html: $(outdir)/%.texi
	-$(MAKEINFO) --force --output=$@ --html --no-split --no-headers $<
# we want footers even if website builds (or is built) partly
	$(footify) $@

# Generic rule not possible?
$(outdir)/%/%.html: $(outdir)/%.texi
	-$(MAKEINFO) --force --output=$@ --html $<
# we want footers even if website builds (or is built) partly
	$(deep-footify) $(sort $(wildcard $(outdir)/$(*F)/*.html))

$(outdir)/%.dvi:	$(outdir)/%.texi
# --clean only in >= 3.12s
# cd $(outdir); texi2dvi --clean ../$< 
	cd $(outdir); texi2dvi ../$< 

$(outdir)/%.txt: $(outdir)/%.texi
	$(MAKEINFO) -I../  --no-split --no-headers --output $@ $<

$(outdir)/%.texi: %.texi
	cp $< $@



