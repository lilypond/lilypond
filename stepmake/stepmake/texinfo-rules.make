
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

# Copy files while tracking their dependencies.
$(outdir)/%.texi: %.texi
	mkdir -p $(dir $@)
	$(DO_TEXI_DEP) cp -f $< $@

$(outdir)/%.itexi: %.itexi
	mkdir -p $(dir $@)
	$(DO_TEXI_DEP) cp -f $< $@

$(outdir)/%.info: $(outdir)/%.texi $(outdir)/$(INFO_IMAGES_DIR).info-images-dir-dep $(outdir)/version.itexi $(outdir)/weblinks.itexi
ifeq ($(WEB_VERSION),yes)
	$(buildscript-dir)/run-and-check "$(MAKEINFO) -I$(src-dir) -I$(outdir) -D web_version --output=$@ $<" "$*.makeinfoweb.log"
else
	$(buildscript-dir)/run-and-check "$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$@ $<" "$*.makeinfo.log"
endif

$(outdir)/%-big-page.html: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.xref-map $(outdir)/version.itexi $(outdir)/weblinks.itexi
ifeq ($(WEB_VERSION),yes)
	$(buildscript-dir)/run-and-check "DEPTH=$(depth) AJAX_SEARCH=$(AJAX_SEARCH) $(TEXI2HTML) $(TEXI2HTML_FLAGS) -D bigpage -D web_version --output=$@ $<"  "$*.bigtexi.log"
else
	$(buildscript-dir)/run-and-check "DEPTH=$(depth) AJAX_SEARCH=$(AJAX_SEARCH) $(TEXI2HTML) $(TEXI2HTML_FLAGS) -D bigpage --output=$@ $<"  "$*.bigtexi.log"
endif

$(outdir)/%.html: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.xref-map $(outdir)/version.itexi $(outdir)/weblinks.itexi
	$(buildscript-dir)/run-and-check "DEPTH=$(depth) AJAX_SEARCH=$(AJAX_SEARCH) $(TEXI2HTML) $(TEXI2HTML_FLAGS) --output=$@ $<"  "$*.texilog.log"


$(outdir)/%/index.html: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.xref-map $(outdir)/version.itexi $(outdir)/weblinks.itexi $(outdir)/%.html.omf
	mkdir -p $(dir $@)
ifeq ($(WEB_VERSION),yes)
	$(buildscript-dir)/run-and-check "DEPTH=$(depth)/../ AJAX_SEARCH=$(AJAX_SEARCH) $(TEXI2HTML) $(TEXI2HTML_SPLIT) $(TEXI2HTML_FLAGS) -D web_version --output=$(dir $@) $<"  "$*.splittexi.log"
else
	$(buildscript-dir)/run-and-check "DEPTH=$(depth)/../ AJAX_SEARCH=$(AJAX_SEARCH) $(TEXI2HTML) $(TEXI2HTML_SPLIT) $(TEXI2HTML_FLAGS) --output=$(dir $@) $<"  "$*.splittexi.log"
endif

ifneq ($(ISOLANG),)
$(XREF_MAPS_DIR)/%.$(ISOLANG).xref-map: $(outdir)/%.texi $(XREF_MAPS_DIR)/%.xref-map
	$(buildscript-dir)/extract_texi_filenames $(XREF_MAP_FLAGS) -o $(XREF_MAPS_DIR) --master-map-file=$(XREF_MAPS_DIR)/$*.xref-map $<
else
$(XREF_MAPS_DIR)/%.xref-map: $(outdir)/%.texi
	$(buildscript-dir)/extract_texi_filenames $(XREF_MAP_FLAGS) -o $(XREF_MAPS_DIR) $<
endif

$(outdir)/%.info: %.texi $(outdir)/$(INFO_IMAGES_DIR).info-images-dir-dep $(outdir)/version.itexi $(outdir)/weblinks.itexi
	$(buildscript-dir)/run-and-check "$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$@ $<"  "$*.makeinfo.log"

$(outdir)/%.pdf: $(outdir)/%.texi $(outdir)/version.itexi $(outdir)/%.pdf.omf $(outdir)/weblinks.itexi
ifeq ($(WEB_VERSION),yes)
	$(buildscript-dir)/run-and-check "cd $(outdir); texi2pdf $(TEXI2PDF_FLAGS) -D web_version -I $(abs-src-dir) $(TEXINFO_PAPERSIZE_OPTION) $(<F) < /dev/null" "$*.texi2pdf.log"
else
	$(buildscript-dir)/run-and-check "cd $(outdir); texi2pdf $(TEXI2PDF_FLAGS) -I $(abs-src-dir) $(TEXINFO_PAPERSIZE_OPTION) $(<F) < /dev/null" "$*.texi2pdf.log"
endif

$(outdir)/%.txt: $(outdir)/%.texi $(outdir)/version.itexi $(outdir)/weblinks.itexi
	$(buildscript-dir)/run-and-check "$(MAKEINFO) -I$(src-dir) -I$(outdir) --no-split --no-headers --output $@ $<"  "$*.makeinfotxt.log"

$(outdir)/%.html.omf: %.texi
	$(call GENERATE_OMF,html)

$(outdir)/%.pdf.omf: %.texi
	$(call GENERATE_OMF,pdf)

$(outdir)/version.%: $(top-src-dir)/VERSION
	$(PYTHON) $(top-src-dir)/scripts/build/create-version-itexi.py > $@

$(outdir)/weblinks.%: $(top-src-dir)/VERSION
	$(PYTHON) $(top-src-dir)/scripts/build/create-weblinks-itexi.py > $@
