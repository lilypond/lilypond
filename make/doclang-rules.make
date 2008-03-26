ifneq "$(ISOLANG)" ""
TEXI2HTML_LANG=--lang=$(ISOLANG)
endif

$(outdir)/%/index.html: $(outdir)/%.texi
	mkdir -p $(dir $@)
	-$(TEXI2HTML) --I=$(outdir) --output=$(outdir)/@(dir $@) --prefix=index --split=section $(TEXI2HTML_LANG) $(TEXI2HTML_INIT) $<

$(outdir)/%-big-page.html: $(outdir)/%.texi
	-$(TEXI2HTML) --I=$(outdir) --output=$@ $(TEXI2HTML_LANG) $(TEXI2HTML_INIT) $< 

$(outdir)/%.pdftexi: $(outdir)/%.texi doc-po
	$(PYTHON) $(buildscript-dir)/texi-gettext.py $(buildscript-dir) $(top-build-dir)/Documentation/po/$(outdir) $(ISOLANG) $<

$(outdir)/%.pdf: $(outdir)/%.pdftexi
	cd $(outdir); texi2pdf $(TEXI2PDF_FLAGS) $(TEXINFO_PAPERSIZE_OPTION) $(notdir $*).pdftexi

$(OUT_TEXI_FILES): $(ITELY_FILES) $(ITEXI_FILES)

$(DEEP_HTML_FILES) $(PDF_FILES): $(ITELY_FILES) $(ITEXI_FILES)

.SECONDARY:
