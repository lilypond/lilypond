# Yodl_rules.make

.SUFFIXES: .1 .5 .7 .dvi .html .latex .ps .texinfo .txt .yo

$(outdir)/%.1: %.yo
	$(YODL2MAN) -doutdir=$(outdir) --live-data=3 -o $@ $<

$(outdir)/%.5: %.yo
	$(YODL2MAN)  -doutdir=$(outdir) --live-data=3 -o $@ $<

$(outdir)/%.7: %.yo
	$(YODL2MAN) -doutdir=$(outdir) --live-data=3 -o $@ $<

$(outdir)/%.html: %.yo
	$(YODL2HTML) -doutdir=$(outdir) --live-data=3 -o $@ $<

$(outdir)/%.latex: %.yo
	$(YODL2LATEX) -doutdir=$(outdir) --live-data=3 -o $@ $<

$(outdir)/%.texinfo: %.yo
	$(YODL2TEXINFO) -doutdir=$(outdir) --live-data=3 -o $@ $<

$(outdir)/%.txt: %.yo
#	$(YODL2TXT) -doutdir=$(outdir) --live-data=3 -o $@ $<
	$(YODL2MSLESS) -doutdir=$(outdir) --live-data=3 $< > $@
	rm -f $(basename $<).ms
