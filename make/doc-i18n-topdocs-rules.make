ifeq (,$(findstring texi2html,$(MISSING_OPTIONAL)))
$(outdir)/%.html: $(outdir)/%.texi
	$(TEXI2HTML) --I=$(src-dir) --I=$(outdir) $(TEXI2HTML_FLAGS) --output=$@ $(TEXI2HTML_INIT) $<
	cp $(top-src-dir)/Documentation/lilypond*.css $(dir $@)
else # Rules using makeinfo follow
$(outdir)/%.html: $(outdir)/%.texi
	$(MAKEINFO) -I$(src-dir) -I$(outdir) -P $(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split --no-headers $< 
endif

$(outdir)/%.pdf: $(outdir)/%.texi
	cd $(outdir); texi2pdf $(TEXI2PDF_FLAGS) $(TEXINFO_PAPERSIZE_OPTION) $(notdir $*).texi

$(outdir)/version.%: $(top-src-dir)/VERSION
	echo '@macro version'> $@
	echo $(TOPLEVEL_VERSION)>> $@
	echo '@end macro'>> $@

$(outdir)/%.txt: $(outdir)/%.texi
	$(MAKEINFO) -I$(src-dir) -I$(outdir) -P $(outdir) --output=$@ --no-split --no-headers $< 
