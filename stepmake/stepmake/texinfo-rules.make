# Texinfo_rules.make

.SUFFIXES: .html .info .texi .texinfo

$(outdir)/%.info: $(outdir)/%.texi
	-$(MAKEINFO) --force -I $(outdir) --output=$@ $<

$(outdir)/%.html: $(outdir)/%.texi
	-$(MAKEINFO) --force -I $(outdir) --output=$@ --html --no-split --no-headers $<
# we want footers even if website builds (or is built) partly
	$(footify) $@

# Generic rule not possible?
$(outdir)/%/%.html: $(outdir)/%.texi
	-$(MAKEINFO) --force --output=$@ --html $<
# we want footers even if website builds (or is built) partly
	$(deep-footify) $(sort $(wildcard $(outdir)/$(*F)/*.html))

$(outdir)/%.dvi: $(outdir)/%.texi
# ugh, --clean removes .. from TEXINPUTS?
#	cd $(outdir); texi2dvi --batch --clean ../$< 
	cd $(outdir); texi2dvi --batch ../$< 

$(outdir)/%.txt: $(outdir)/%.texi
	$(MAKEINFO) --force -I../ -I $(outdir) --no-split --no-headers --output $@ $<

$(outdir)/%.texi: %.texi
	rm -f $@
	cp $< $@
	chmod -w $@



