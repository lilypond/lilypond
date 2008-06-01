.SUFFIXES: .doc .tely .texi .ly

# TODO: fix hardcoded out/ ?
LYS_OUTPUT_OPTION= --lily-output-dir $(LYS_OUTPUT_DIR)
LYS_OUTPUT_DIR=$(top-build-dir)/out/lybook-db/
LILYPOND_BOOK_FLAGS += $(LYS_OUTPUT_OPTION)
$(outdir)/%.latex:  %.doc
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND_BOOK_PROCESS) $(LILYPOND_BOOK_LILYPOND_FLAGS)' --output=$(outdir) $(LILYPOND_BOOK_FLAGS) $<



# don't do ``cd $(outdir)'', and assume that $(outdir)/.. is the src dir.
# it is not, for --srcdir builds
$(outdir)/%.texi: %.tely $(outdir)/version.itexi
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND_BOOK_PROCESS) $(LILYPOND_BOOK_LILYPOND_FLAGS)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) $(LILYPOND_BOOK_FLAGS) $<


$(outdir)/%.texi: $(outdir)/%.tely $(outdir)/version.itexi
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND_BOOK_PROCESS) $(LILYPOND_BOOK_INCLUDES) $(LILYPOND_BOOK_LILYPOND_FLAGS)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) $(LILYPOND_BOOK_FLAGS) $<

$(outdir)/%.html.omf: %.tely
	$(call GENERATE_OMF,html)

$(outdir)/%.pdf.omf: %.tely
	$(call GENERATE_OMF,pdf)

$(outdir)/%.ps.gz.omf: %.tely
	$(call GENERATE_OMF,ps.gz)

$(outdir)/%.html.omf: $(outdir)/%.texi
	$(call GENERATE_OMF,html)

$(outdir)/%.pdf.omf: $(outdir)/%.texi
	$(call GENERATE_OMF,pdf)

$(outdir)/%.ps.gz.omf: $(outdir)/%.texi
	$(call GENERATE_OMF,ps.gz)
