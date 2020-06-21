depth = ../../..

STEPMAKE_TEMPLATES=texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc lilypond-book

COLLATED_FILES = $(sort $(filter-out %.xml,$(OUT_FILES)))

include $(depth)/make/stepmake.make

TITLE=lilypond-book Test Suite

OUT_TEXI_FILES += ${TEXI_FILES:%.texi=$(outdir)/%.info}
OUT_TEXI_FILES += ${TEXI_FILES:%.texi=$(outdir)/%.pdf}
OUT_TEXINFO_FILES += ${TEXINFO_FILES:%.texinfo=$(outdir)/%.info}
OUT_TEXINFO_FILES += ${TEXINFO_FILES:%.texinfo=$(outdir)/%.pdf}
OUT_TELY_FILES += ${TELY_FILES:%.tely=$(outdir)/%.info}
OUT_TELY_FILES += ${TELY_FILES:%.tely=$(outdir)/%.pdf}

XML_FILES = $(filter-out include%,$(call src-wildcard,*.xml))

local-test:
	$(MAKE) LYS_OUTPUT_DIR=$(top-build-dir)/out/lybook-testdb $(OUT_FILES)

$(outdir)/suffix-texi.texi: suffix-texi.texi $(outdir)/version.itexi $(DOCUMENTATION_LOCALE_TARGET) $(INIT_LY_SOURCES) $(SCHEME_SOURCES)
	$(call ly_progress,Making,$@,)
	LILYPOND_VERSION=$(TOPLEVEL_VERSION) \
		TEX=$(PDFTEX) PDFTEX=$(PDFTEX) PDFLATEX=$(PDFLATEX) \
		$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) \
		--process='$(LILYPOND_BOOK_PROCESS) \
		$(LILYPOND_BOOK_LILYPOND_FLAGS)' \
		--output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) \
		$(LILYPOND_BOOK_FLAGS) --redirect-lilypond-output $<

# Due to automatic language detection the standard rule using the script
# extract_texi_filenames would create .es.xref-map for these files. The
# following rule is to circumvent this for the purpose of the build. For
# the regression tests the .xref-map files are empty and not used anyway.
$(XREF_MAPS_DIR)/texinfo-include-language-detection.xref-map $(XREF_MAPS_DIR)/texinfo-language-detection.xref-map: %:
	mkdir -p $(dir $@) ; touch $@
