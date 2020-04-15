.SUFFIXES: .doc .tely .texi .ly

# TODO: fix hardcoded out/ ?
LYS_OUTPUT_OPTION= --lily-output-dir $(LYS_OUTPUT_DIR)
LYS_OUTPUT_DIR=$(top-build-dir)/out/lybook-db
LILYPOND_BOOK_FLAGS += $(LYS_OUTPUT_OPTION)
$(outdir)/%.latex: %.doc $(INIT_LY_SOURCES) $(SCHEME_SOURCES)
	$(call ly_progress,Making,$@,< doc)
	LILYPOND_VERSION=$(TOPLEVEL_VERSION) \
		TEX=$(PDFTEX) PDFTEX=$(PDFTEX) PDFLATEX=$(PDFLATEX) \
		$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) \
		--process='$(LILYPOND_BOOK_PROCESS) \
		$(LILYPOND_BOOK_LILYPOND_FLAGS)' \
		--output=$(outdir) $(LILYPOND_BOOK_FLAGS) \
		--redirect-lilypond-output $<

$(eval $(firstword $(TEXI_FILES_FROM_TELY)):\
 $(foreach i, $(wordlist 2, $(words $(TEXI_FILES_FROM_TELY)),\
 $(TEXI_FILES_FROM_TELY)),$(CHAIN_RULE)))

# don't do ``cd $(outdir)'', and assume that $(outdir)/.. is the src dir.
# it is not, for --srcdir builds
$(outdir)/%.texi: %.tely $(outdir)/version.itexi $(DOCUMENTATION_LOCALE_TARGET) $(INIT_LY_SOURCES) $(SCHEME_SOURCES)
	$(call ly_progress,Making,$@,< tely)
	LILYPOND_VERSION=$(TOPLEVEL_VERSION) \
		TEX=$(PDFTEX) PDFTEX=$(PDFTEX) PDFLATEX=$(PDFLATEX) \
		$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) \
		--process='$(LILYPOND_BOOK_PROCESS) \
		$(LILYPOND_BOOK_LILYPOND_FLAGS)' \
		--output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) \
		$(LILYPOND_BOOK_FLAGS) --redirect-lilypond-output $<


$(outdir)/%.texi: $(outdir)/%.tely $(outdir)/version.itexi $(DOCUMENTATION_LOCALE_TARGET) $(INIT_LY_SOURCES) $(SCHEME_SOURCES)
	$(call ly_progress,Making,$@,< tely)
	LILYPOND_VERSION=$(TOPLEVEL_VERSION) \
		TEX=$(PDFTEX) PDFTEX=$(PDFTEX) PDFLATEX=$(PDFLATEX) \
		$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) \
		--process='$(LILYPOND_BOOK_PROCESS) \
		$(LILYPOND_BOOK_INCLUDES) $(LILYPOND_BOOK_LILYPOND_FLAGS)' \
		--output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) \
		$(LILYPOND_BOOK_FLAGS) --redirect-lilypond-output $<


$(outdir)/others-did.itexi $(outdir)/we-wrote.itexi: $(outdir)/%.itexi: $(top-src-dir)/Documentation/web/%.bib $(top-src-dir)/Documentation/lily-bib.bst
	$(call ly_progress,Making,$@,)
	BSTINPUTS=$(top-src-dir)/Documentation/web/ $(PYTHON) $(buildscript-dir)/bib2texi.py \
		-s $(top-src-dir)/Documentation/lily-bib \
		-o $(outdir)/$*.itexi \
		-q \
		$(top-src-dir)/Documentation/web/$*.bib
