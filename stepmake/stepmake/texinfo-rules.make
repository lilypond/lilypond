
.SUFFIXES: .html .info .texi .texinfo

# "makeinfo --info" MUST be able to read PNGs from CWD for info images
# to work, hence $(INFO_IMAGES_DIR) -> $(outdir)/ symlink.
# $(outdir)/$(INFO_IMAGES_DIR)/*.png symlinks are only needed to view
# out-www/*.info with Emacs -- HTML docs no longer need these
# symlinks, see replace_symlinks_urls in
# buildscripts/add_html_footer.py.

ifneq ($(INFO_IMAGES_DIR),)

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


# TODO: Pass -D bigpage to texi2html
$(outdir)/%-big-page.html: $(outdir)/%.texi
	$(TEXI2HTML) --I=$(outdir) --output=$@ $(TEXI2HTML_LANG) $< 

$(outdir)/%.html: $(outdir)/%.texi
	$(TEXI2HTML) --I=$(outdir) --output=$@ $(TEXI2HTML_LANG) $<

$(outdir)/%.html.omf: %.texi
	$(call GENERATE_OMF,html)

$(outdir)/%.pdf.omf: %.texi
	$(call GENERATE_OMF,pdf)

$(outdir)/%.ps.gz.omf: %.texi
	$(call GENERATE_OMF,ps.gz)

$(outdir)/%/index.html: $(outdir)/%.texi
	mkdir -p $(dir $@)
	$(TEXI2HTML) --I=$(outdir) --output=$(dir $@) $(TEXI2HTML_LANG) --split=section $<

$(outdir)/%.pdf: $(outdir)/%.texi
	cd $(outdir); texi2pdf $(TEXI2PDF_FLAGS) --batch $(TEXINFO_PAPERSIZE_OPTION) $(<F)

$(outdir)/%.txt: $(outdir)/%.texi
	$(MAKEINFO) -I $(src-dir) -I $(outdir) --no-split --no-headers --output $@ $<

$(outdir)/%.texi: %.texi
	rm -f $@
	cp $< $@



