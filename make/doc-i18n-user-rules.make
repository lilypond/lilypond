$(outdir)/%/index.html: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.$(ISOLANG).xref-map $(OUT_PNG_IMAGES) $(CSS_SOURCE_FILES)
	mkdir -p $(dir $@)
	$(TEXI2HTML) --I=$(src-dir) --I=$(outdir) $(TEXI2HTML_FLAGS) --output=$(dir $@) --prefix=index --split=section $(TEXI2HTML_INIT) $<
	cp  $(CSS_SOURCE_FILES) $(dir $@)

$(outdir)/%-big-page.html: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.$(ISOLANG).xref-map $(OUT_PNG_IMAGES) $(CSS_SOURCE_FILES)
	$(TEXI2HTML) --I=$(src-dir) --I=$(outdir) -D bigpage $(TEXI2HTML_FLAGS) --output=$@ $(TEXI2HTML_INIT) $<
	cp  $(CSS_SOURCE_FILES) $(dir $@)

$(outdir)/%.pdftexi: $(outdir)/%.texi
	$(buildscript-dir)/texi-gettext $(ISOLANG) $<

$(outdir)/%.pdf: $(outdir)/%.pdftexi
	cd $(outdir); texi2pdf $(TEXI2PDF_FLAGS) $(TEXINFO_PAPERSIZE_OPTION) $(notdir $*).pdftexi

$(outdir)/version.%: $(top-src-dir)/VERSION
	echo '@macro version'> $@
	echo $(TOPLEVEL_VERSION)>> $@
	echo '@end macro'>> $@

$(outdir)/%.png: $(top-build-dir)/Documentation/user/$(outdir)/%.png
	ln -f $< $@

$(XREF_MAPS_DIR)/%.$(ISOLANG).xref-map: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.xref-map
	$(buildscript-dir)/extract_texi_filenames -o $(XREF_MAPS_DIR) --master-map-file=$(XREF_MAPS_DIR)/$*.xref-map $<

$(MASTER_TEXI_FILES): $(ITELY_FILES) $(ITEXI_FILES)

.SECONDARY:
