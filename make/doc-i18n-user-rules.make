ifeq (,$(findstring texi2html,$(MISSING_OPTIONAL)))
$(outdir)/%/index.html: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.$(ISOLANG).xref-map $(OUT_PNG_IMAGES)
	mkdir -p $(dir $@)
	$(TEXI2HTML) --I=$(src-dir) --I=$(outdir) $(TEXI2HTML_FLAGS) --output=$(dir $@) --prefix=index --split=section $(TEXI2HTML_INIT) $<
	cp $(top-src-dir)/Documentation/lilypond*.css $(dir $@)

$(outdir)/%-big-page.html: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.$(ISOLANG).xref-map $(OUT_PNG_IMAGES)
	$(TEXI2HTML) --I=$(src-dir) --I=$(outdir) -D bigpage $(TEXI2HTML_FLAGS) --output=$@ $(TEXI2HTML_INIT) $<
	cp $(top-src-dir)/Documentation/lilypond*.css $(dir $@)
else # Rules using makeinfo follow
$(outdir)/%/index.html: $(outdir)/%.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(src-dir) -I$(outdir) -P $(outdir) --output=$(outdir)/$* --css-include=$(top-src-dir)/Documentation/texinfo.css --html $<

$(outdir)/%-big-page.html: $(outdir)/%.texi
	$(MAKEINFO) -I$(src-dir) -I$(outdir) -P $(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split --no-headers $< 
endif

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

$(XREF_MAPS_DIR)/%.$(ISOLANG).xref-map: $(outdir)/%.texi
	$(buildscript-dir)/extract_texi_filenames -o $(XREF_MAPS_DIR) $<

$(MASTER_TEXI_FILES): $(ITELY_FILES) $(ITEXI_FILES)

.SECONDARY:
