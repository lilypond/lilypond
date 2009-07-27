$(top-build-dir)/Documentation/$(outdir)/%/index.$(ISOLANG).html: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.$(ISOLANG).xref-map $(TRANSLATION_LILY_IMAGES)
	mkdir -p $(dir $@)
	mkdir -p $(outdir)/$*
	$(TEXI2HTML) --I=$(src-dir) --I=$(outdir) $(TEXI2HTML_FLAGS) --output=$(outdir)/$* --prefix=index --split=section $(TEXI2HTML_INIT) $<
	find $(outdir)/$* -name '*.html' | xargs grep -L --label="" 'UNTRANSLATED NODE: IGNORE ME' | sed 's!$(outdir)/!!g' | xargs $(buildscript-dir)/mass-link --prepend-suffix .$(ISOLANG) hard $(outdir) $(top-build-dir)/Documentation/$(outdir)


$(top-build-dir)/Documentation/$(outdir)/%-big-page.$(ISOLANG).html: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.$(ISOLANG).xref-map $(TRANSLATION_LILY_IMAGES)
	$(TEXI2HTML) --I=$(src-dir) --I=$(outdir) -D bigpage $(TEXI2HTML_FLAGS) --output=$@ $(TEXI2HTML_INIT) $<


$(top-build-dir)/Documentation/$(outdir)/%.$(ISOLANG).pdf: $(outdir)/%.texi
	cd $(outdir) && \
	texi2pdf $(TEXI2PDF_FLAGS) $(TEXINFO_PAPERSIZE_OPTION) $*.texi && \
	mkdir -p $(dir $@) && mv $*.pdf $@

$(outdir)/version.%: $(top-src-dir)/VERSION
	echo '@macro version'> $@
	echo $(TOPLEVEL_VERSION)>> $@
	echo '@end macro'>> $@

$(outdir)/%.png: $(top-build-dir)/Documentation/$(outdir)/%.png
	ln -f $< $@

$(XREF_MAPS_DIR)/%.$(ISOLANG).xref-map: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.xref-map
	$(buildscript-dir)/extract_texi_filenames -o $(XREF_MAPS_DIR) $(XREF_MAP_FLAGS) --master-map-file=$(XREF_MAPS_DIR)/$*.xref-map $<

$(MASTER_TEXI_FILES): $(ITELY_FILES) $(ITEXI_FILES)

$(TRANSLATION_LILY_IMAGES): $(MASTER_TEXI_FILES)
	find $(outdir) \( -name 'lily-*.png' -o -name 'lily-*.ly' \) | sed 's!$(outdir)/!!g' | xargs $(buildscript-dir)/mass-link hard $(outdir) $(top-build-dir)/Documentation/$(outdir)
	touch $@

.SECONDARY:
