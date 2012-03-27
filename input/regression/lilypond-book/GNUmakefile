depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc lilypond-book

include $(depth)/make/stepmake.make

TITLE=lilypond-book Test Suite

SUBDIRS=include

COLLATED_FILES = $(OUT_FILES)

EXTRA_DIST_FILES = include.mxl
EXTRA_DIST_FILES += include.xml
EXTRA_DIST_FILES += $(call src-wildcard,*.ly)
EXTRA_DIST_FILES += $(call src-wildcard,*.ily)
EXTRA_DIST_FILES += $(call src-wildcard,*.tely)
EXTRA_DIST_FILES += $(call src-wildcard,*.itely)

OUT_TEXI_FILES += ${TEXI_FILES:%.texi=$(outdir)/%.info}
OUT_TEXI_FILES += ${TEXI_FILES:%.texi=$(outdir)/%.pdf}
OUT_TEXINFO_FILES += ${TEXINFO_FILES:%.texinfo=$(outdir)/%.info}
OUT_TEXINFO_FILES += ${TEXINFO_FILES:%.texinfo=$(outdir)/%.pdf}
OUT_TELY_FILES += ${TELY_FILES:%.tely=$(outdir)/%.info}
OUT_TELY_FILES += ${TELY_FILES:%.tely=$(outdir)/%.pdf}

XML_FILES = $(filter-out include%,$(call src-wildcard,*.xml))

local-test: $(OUT_FILES)

$(outdir)/collated-files.list: $(OUT_FILES)
	echo $(sort $(filter-out %.xml,$(OUT_FILES))) > $@

$(outdir)/suffix-texi.texi: suffix-texi.texi $(outdir)/version.itexi $(DOCUMENTATION_LOCALE_TARGET) $(INIT_LY_SOURCES) $(SCHEME_SOURCES)
	LILYPOND_VERSION=$(TOPLEVEL_VERSION) $(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND_BOOK_PROCESS) $(LILYPOND_BOOK_LILYPOND_FLAGS)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) $(LILYPOND_BOOK_FLAGS) --redirect-lilypond-output $<

# Due to automatic language detection the standard rule using the script
# extract_texi_filenames would create .es.xref-map for these files. The
# following rule is to circumvent this for the purpose of the build. For
# the regression tests the .xref-map files are empty and not used anyway.
$(XREF_MAPS_DIR)/texinfo-include-language-detection.xref-map $(XREF_MAPS_DIR)/texinfo-language-detection.xref-map: %:
	touch $@

# Prevent parallel lilypond-book instances for this subdir
.NOTPARALLEL:
