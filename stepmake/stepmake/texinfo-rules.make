
.SUFFIXES: .html .info .texi .texinfo

$(outdir)/%.info: $(outdir)/%.texi
	$(MAKEINFO) -I $(outdir) --output=$@ $<

$(outdir)/%.html: $(outdir)/%.texi
	$(MAKEINFO) -I $(outdir) --output=$@ --html --no-split --no-headers $<
# we want footers even if website builds (or is built) partly
	$(footify) $@


$(outdir)/%.html.omf: $(outdir)/%.texi
	$(GENERATE_OMF) --format html --location $(local_package_docdir)/$(current-relative-dir)/out-www/$(notdir $(basename $@))  --version $(TOPLEVEL_VERSION) $< > $@
$(outdir)/%.pdf.omf: $(outdir)/%.texi
	$(GENERATE_OMF) --format pdf --location $(local_package_docdir)/$(current-relative-dir)/out-www/$(notdir $(basename $@))  --version $(TOPLEVEL_VERSION) $< > $@

$(outdir)/%.ps.gz.omf: $(outdir)/%.texi
	$(GENERATE_OMF) --format ps.gz --location $(local_package_docdir)/$(current-relative-dir)/out-www/$(notdir $(basename $@))  --version $(TOPLEVEL_VERSION) $< > $@

# Generic rule not possible?
$(outdir)/%/%.html: $(outdir)/%.texi
	$(MAKEINFO) --output=$@ --html $<
# we want footers even if website builds (or is built) partly
	$(deep-footify) $(sort $(wildcard $(outdir)/$(*F)/*.html))

$(outdir)/%.dvi: $(outdir)/%.texi
	cd $(outdir); texi2dvi --batch $(<F)

$(outdir)/%.txt: $(outdir)/%.texi
	$(MAKEINFO) -I $(pwd) -I $(outdir) --no-split --no-headers --output $@ $<

$(outdir)/%.texi: %.texi
	rm -f $@
	cp $< $@
	chmod -w $@



