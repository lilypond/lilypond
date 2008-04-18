$(outdir)/%/index.html: $(outdir)/%.texi $(outdir)/version.itexi
	mkdir -p $(dir $@)
	-$(MAKEINFO) -P $(outdir) --output=$(outdir)/$* --css-include=$(top-src-dir)/Documentation/texinfo.css --html $<

$(outdir)/%-big-page.html: $(outdir)/%.texi $(outdir)/version.itexi
	-$(MAKEINFO) -P $(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split --no-headers $< 

$(outdir)/%.pdftexi: $(outdir)/%.texi doc-po $(outdir)/version.itexi
	$(PYTHON) $(buildscript-dir)/texi-gettext.py $(buildscript-dir) $(top-build-dir)/Documentation/po/$(outdir) $(ISOLANG) $<

$(outdir)/%.pdf: $(outdir)/%.pdftexi
	cd $(outdir); texi2pdf $(TEXI2PDF_FLAGS) $(TEXINFO_PAPERSIZE_OPTION) $(notdir $*).pdftexi

$(outdir)/version.%: $(top-src-dir)/VERSION
	echo '@macro version'> $@
	echo $(TOPLEVEL_VERSION)>> $@
	echo '@end macro'>> $@

$(OUT_TEXI_FILES): $(ITELY_FILES) $(ITEXI_FILES)

$(DEEP_HTML_FILES) $(PDF_FILES): $(ITELY_FILES) $(ITEXI_FILES)

.SECONDARY:
