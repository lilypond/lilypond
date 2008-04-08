
.SUFFIXES: .html .info .texi .texinfo

# "makeinfo --info" MUST be able to read PNGs from CWD for info images
# to work, hence $(INFO_IMAGES_DIR) -> $(outdir)/ symlink.
# $(outdir)/$(INFO_IMAGES_DIR)/*.png symlinks are only needed to view
# out-www/*.info with Emacs -- HTML docs no longer need these
# symlinks, see replace_symlinks_urls in
# buildscripts/add_html_footer.py.

ifneq ($(INFO_IMAGES_DIR),'')

# make dereferences symlinks, and $(INFO_IMAGES_DIR) is a symlink
# to $(outdir), so we can't use directly $(INFO_IMAGES_DIR) as a
# prerequisite, otherwise %.info are always outdated (because older
# than $(outdir), hence this .dep file

$(outdir)/%.info-images-dir.dep: $(outdir)/%.texi
	rm -f $*
	ln -s $(outdir) $*
	mkdir -p $(outdir)/$*
	find $(outdir)/$*/ -name '*'.png | xargs rm -f
	(cd $(outdir)/$*/ ; ln -sf ../*.png . )
	touch $@

else

$(outdir)/.info-images-dir.dep:
	touch $@

endif

$(outdir)/%.info: $(outdir)/%.texi $(outdir)/$(INFO_IMAGES_DIR).info-images-dir.dep
	$(MAKEINFO) -I$(outdir) --output=$@ $<

$(outdir)/%-big-page.html: $(outdir)/%.texi
	$(MAKEINFO) -I $(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split -D bigpage --no-headers $<

$(outdir)/%.html: $(outdir)/%.texi
	$(MAKEINFO) -I $(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split --no-headers $<

$(outdir)/%.html.omf: %.texi
	$(call GENERATE_OMF,html)

$(outdir)/%.pdf.omf: %.texi
	$(call GENERATE_OMF,pdf)

$(outdir)/%.ps.gz.omf: %.texi
	$(call GENERATE_OMF,ps.gz)

$(outdir)/%/index.html: $(outdir)/%.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) -I $(outdir) --output=$(dir $@) --css-include=$(top-src-dir)/Documentation/texinfo.css --html $<

$(outdir)/%.pdf: $(outdir)/%.texi
	cd $(outdir); texi2pdf $(TEXI2PDF_FLAGS) --batch $(TEXINFO_PAPERSIZE_OPTION) $(<F)

$(outdir)/%.txt: $(outdir)/%.texi
	$(MAKEINFO) -I $(src-dir) -I $(outdir) --no-split --no-headers --output $@ $<

$(outdir)/%.texi: %.texi
	rm -f $@
	cp $< $@



