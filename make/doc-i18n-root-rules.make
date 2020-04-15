
.SUFFIXES: .html .info .texi .texinfo

# Explicitly list the dependencies on generated content
$(outdir)/web.texi: $(outdir)/we-wrote.itexi $(outdir)/others-did.itexi $(outdir)/weblinks.itexi $(outdir)/version.itexi

$(top-build-dir)/Documentation/$(outdir)/%/index.$(ISOLANG).html: $(outdir)/%/index.html $(TRANSLATION_LILY_IMAGES)
	$(call ly_progress,Making,$@,(hard links))
	mkdir -p $(dir $@)
	find $(outdir)/$* -name '*.html' | xargs grep -L 'UNTRANSLATED NODE: IGNORE ME' | sed 's!$(outdir)/!!g' | xargs $(buildscript-dir)/mass-link --prepend-suffix .$(ISOLANG) hard $(outdir) $(top-build-dir)/Documentation/$(outdir)

$(top-build-dir)/Documentation/$(outdir)/%-big-page.$(ISOLANG).html: $(outdir)/%-big-page.html $(TRANSLATION_LILY_IMAGES)
	$(call ly_progress,Making,$@,(hard link))
	mkdir -p $(dir $@)
	ln -f $< $@

$(top-build-dir)/Documentation/$(outdir)/%.$(ISOLANG).html: $(outdir)/%.html
	$(call ly_progress,Making,$@,(hard link))
	mkdir -p $(dir $@)
	ln -f $< $@

$(top-build-dir)/Documentation/$(outdir)/%.$(ISOLANG).pdf: $(outdir)/%.pdf
	$(call ly_progress,Making,$@,(hard link))
	mkdir -p $(dir $@)
	ln -f $< $@

$(outdir)/%.png: $(top-build-dir)/Documentation/$(outdir)/%.png
	$(call ly_progress,Making,$@,(hard link))
	ln -f $< $@

$(MASTER_TEXI_FILES): $(ITELY_FILES) $(ITEXI_FILES) $(outdir)/pictures $(outdir)/ly-examples

$(outdir)/pictures:
	$(MAKE) -C $(top-build-dir)/Documentation/pictures out=www WWW-1
	ln -sf $(top-build-dir)/Documentation/pictures/$(outdir) $@

$(outdir)/ly-examples:
	$(MAKE) -C $(top-build-dir)/Documentation/ly-examples out=www WWW-1
	ln -sf $(top-build-dir)/Documentation/ly-examples/$(outdir) $@

$(TRANSLATION_LILY_IMAGES): $(MASTER_TEXI_FILES)
	$(call ly_progress,Making,$@,(hard links))
	find $(outdir) \( -name 'lily-*.png' -o -name 'lily-*.ly' \) | sed 's!$(outdir)/!!g' | xargs $(buildscript-dir)/mass-link hard $(outdir) $(top-build-dir)/Documentation/$(outdir)
	find $(outdir) \( -name '*.??.idx' \) | sed 's!$(outdir)/!!g' | xargs $(buildscript-dir)/mass-link hard $(outdir) $(top-build-dir)/Documentation/$(outdir)
	touch $@
