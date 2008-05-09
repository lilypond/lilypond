<<<<<<< HEAD:make/doclang-rules.make
$(outdir)/%/index.html: $(outdir)/%.texi $(OUT_PNG_IMAGES) $(outdir)/version.itexi
	mkdir -p $(dir $@)
	$(TEXI2HTML) --I=$(outdir) $(TEXI2HTML_FLAGS) --output=$(dir $@) --prefix=index --split=section $(TEXI2HTML_INIT) $<
	cp $(top-src-dir)/Documentation/lilypond*.css $(dir $@)

$(outdir)/%-big-page.html: $(outdir)/%.texi $(OUT_PNG_IMAGES) $(outdir)/version.itexi
	$(TEXI2HTML) --I=$(outdir) $(TEXI2HTML_FLAGS) --output=$@ $(TEXI2HTML_INIT) $<
	cp $(top-src-dir)/Documentation/lilypond*.css $(dir $@)

$(outdir)/%.pdftexi: $(outdir)/%.texi doc-po $(outdir)/version.itexi
	$(PYTHON) $(buildscript-dir)/texi-gettext.py $(ISOLANG) $<

$(outdir)/%.pdf: $(outdir)/%.pdftexi $(outdir)/version.itexi
	cd $(outdir); texi2pdf $(TEXI2PDF_FLAGS) $(TEXINFO_PAPERSIZE_OPTION) $(notdir $*).pdftexi

$(outdir)/version.%: $(top-src-dir)/VERSION
	echo '@macro version'> $@
	echo $(TOPLEVEL_VERSION)>> $@
	echo '@end macro'>> $@

$(outdir)/%.png: $(top-build-dir)/Documentation/user/$(outdir)/%.png
	ln -f $< $@

# This makes sure lilypond-doc gettext domain has been compiled
# before lilypond-book runs
%.tely: doc-po

$(OUT_TEXI_FILES): $(ITELY_FILES) $(ITEXI_FILES)

$(DEEP_HTML_FILES) $(PDF_FILES): $(ITELY_FILES) $(ITEXI_FILES)

.SECONDARY:
