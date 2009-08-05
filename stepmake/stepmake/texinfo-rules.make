
.SUFFIXES: .html .info .texi .texinfo

# "makeinfo --info" MUST be able to read PNGs from CWD for info images
# to work, hence $(INFO_IMAGES_DIR) -> $(outdir)/ symlink.
# $(outdir)/$(INFO_IMAGES_DIR)/*.png symlinks are only needed to view
# out-www/*.info with Emacs -- HTML docs no longer need these
# symlinks, see replace_symlinks_urls in
# python/auxiliar/postprocess_html.py.

# make dereferences symlinks, and $(INFO_IMAGES_DIR) is a symlink
# to $(outdir), so we can't use directly $(INFO_IMAGES_DIR) as a
# prerequisite, otherwise %.info are always outdated (because older
# than $(outdir)), hence this .dep file

$(outdir)/$(INFO_IMAGES_DIR).info-images-dir-dep: $(OUT_TEXI_FILES)
ifneq ($(INFO_IMAGES_DIR),)
	rm -f $(INFO_IMAGES_DIR)
	ln -s $(outdir) $(INFO_IMAGES_DIR)
	mkdir -p $(outdir)/$(INFO_IMAGES_DIR)
	rm -f $(outdir)/$(INFO_IMAGES_DIR)/[a-f0-9][a-f0-9]
	cd $(outdir)/$(INFO_IMAGES_DIR) && $(buildscript-dir)/mass-link symbolic .. . [a-f0-9][a-f0-9]
endif
	touch $@

$(outdir)/%.info: $(outdir)/%.texi $(outdir)/$(INFO_IMAGES_DIR).info-images-dir-dep $(outdir)/version.itexi
	$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$@ $<

$(outdir)/lilypond-%.info: $(outdir)/%.texi $(outdir)/$(INFO_IMAGES_DIR).info-images-dir-dep $(outdir)/version.itexi
	$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$@ $<

$(outdir)/%-big-page.html: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.xref-map $(outdir)/version.itexi
	$(TEXI2HTML) --I=$(src-dir) --I=$(outdir) -D bigpage --output=$@ $(TEXI2HTML_INIT) $<

$(outdir)/%.html: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.xref-map $(outdir)/version.itexi
	$(TEXI2HTML) --I=$(src-dir) --I=$(outdir) --output=$@ $(TEXI2HTML_INIT) $<

$(outdir)/%/index.html: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.xref-map $(outdir)/version.itexi $(outdir)/%.html.omf
	mkdir -p $(dir $@)
	$(TEXI2HTML) --I=$(src-dir) --I=$(outdir) --output=$(dir $@) --prefix=index --split=section $(TEXI2HTML_INIT) $<
	cp $(top-src-dir)/Documentation/css/*.css $(dir $@)


$(outdir)/%.html.omf: %.texi
	$(call GENERATE_OMF,html)

$(outdir)/%.pdf.omf: %.texi
	$(call GENERATE_OMF,pdf)

$(outdir)/%.pdf: $(outdir)/%.texi $(outdir)/version.itexi $(outdir)/%.pdf.omf
	cd $(outdir); texi2pdf $(TEXI2PDF_FLAGS) -I $(abs-src-dir) --batch $(TEXINFO_PAPERSIZE_OPTION) $(<F)

$(outdir)/%.txt: $(outdir)/%.texi $(outdir)/version.itexi
	$(MAKEINFO) -I$(src-dir) -I$(outdir) --no-split --no-headers --output $@ $<

$(XREF_MAPS_DIR)/%.xref-map: $(outdir)/%.texi
	$(buildscript-dir)/extract_texi_filenames $(XREF_MAP_FLAGS) -o $(XREF_MAPS_DIR) $<

$(outdir)/%.texi: %.texi
	cp -f $< $@

$(outdir)/version.%: $(top-src-dir)/VERSION
	echo '@macro version'> $@
	echo $(TOPLEVEL_VERSION)>> $@
	echo '@end macro'>> $@

.SECONDARY: $(outdir)/version.itexi $(outdir)/version.texi \
  $(outdir)/$(INFO_IMAGES_DIR).info-images-dir-dep \
  $(outdir)/*.texi
