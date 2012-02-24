
.SUFFIXES: .html .info .texi .texinfo

# Explicitly list the dependencies on generated content
$(outdir)/web.texi: $(outdir)/weblinks.itexi

$(top-build-dir)/Documentation/$(outdir)/%/index.$(ISOLANG).html: $(outdir)/%/index.html $(TRANSLATION_LILY_IMAGES)
	mkdir -p $(dir $@)
	find $(outdir)/$* -name '*.html' | xargs grep -L 'UNTRANSLATED NODE: IGNORE ME' | sed 's!$(outdir)/!!g' | xargs $(buildscript-dir)/mass-link --prepend-suffix .$(ISOLANG) hard $(outdir) $(top-build-dir)/Documentation/$(outdir)

$(top-build-dir)/Documentation/$(outdir)/%-big-page.$(ISOLANG).html: $(outdir)/%-big-page.html $(TRANSLATION_LILY_IMAGES)
	mkdir -p $(dir $@)
	cp -f $< $@

$(top-build-dir)/Documentation/$(outdir)/%.$(ISOLANG).html: $(outdir)/%.html
	mkdir -p $(dir $@)
	cp -f $< $@

$(top-build-dir)/Documentation/$(outdir)/%.$(ISOLANG).pdf: $(outdir)/%.pdf
	mkdir -p $(dir $@)
	cp -f $< $@

$(outdir)/version.%: $(top-src-dir)/VERSION
	$(PYTHON) $(top-src-dir)/scripts/build/create-version-itexi.py > $@

$(outdir)/weblinks.%: $(top-src-dir)/VERSION
	$(PYTHON) $(top-src-dir)/scripts/build/create-weblinks-itexi.py > $@

$(outdir)/%.png: $(top-build-dir)/Documentation/$(outdir)/%.png
	ln -f $< $@

$(MASTER_TEXI_FILES): $(ITELY_FILES) $(ITEXI_FILES) $(outdir)/pictures

$(outdir)/pictures:
	$(MAKE) -C $(top-build-dir)/Documentation/pictures WWW-1
	ln -sf $(top-build-dir)/Documentation/pictures/$(outdir) $@

$(TRANSLATION_LILY_IMAGES): $(MASTER_TEXI_FILES)
	find $(outdir) \( -name 'lily-*.png' -o -name 'lily-*.ly' \) | sed 's!$(outdir)/!!g' | xargs $(buildscript-dir)/mass-link hard $(outdir) $(top-build-dir)/Documentation/$(outdir)
	find $(outdir) \( -name '*.??.idx' \) | sed 's!$(outdir)/!!g' | xargs $(buildscript-dir)/mass-link hard $(outdir) $(top-build-dir)/Documentation/$(outdir)
	touch $@

$(outdir)/lilypond-%.info: $(outdir)/%.texi $(outdir)/$(INFO_IMAGES_DIR).info-images-dir-dep $(outdir)/version.itexi $(outdir)/weblinks.itexi
	$(buildscript-dir)/run-and-check "$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$@ $<" "$*.makeinfo.log"

$(outdir)/index.$(ISOLANG).html: TEXI2HTML_INIT = $(WEB_TEXI2HTML_INIT)
$(outdir)/index.$(ISOLANG).html: TEXI2HTML_SPLIT = $(WEB_TEXI2HTML_SPLIT)

$(outdir)/index.$(ISOLANG).html:
	$(buildscript-dir)/run-and-check "DEPTH=$(depth) $(TEXI2HTML) $(TEXI2HTML_FLAGS) $(TEXI2HTML_SPLIT) --output=$(outdir)/ web.texi" "webtexi.log"
	find $(outdir)/ -name '*.html' | xargs grep -L 'UNTRANSLATED NODE: IGNORE ME' | sed 's!$(outdir)/!!g' | xargs $(buildscript-dir)/mass-link --prepend-suffix .$(ISOLANG) hard $(outdir) $(top-build-dir)/Documentation/$(outdir)
