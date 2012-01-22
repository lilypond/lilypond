depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc lilypond-book

include $(depth)/make/stepmake.make

TITLE=lilypond-book Test Suite

COLLATED_FILES = $(OUT_FILES)

EXTRA_DIST_FILES = include.mxl
EXTRA_DIST_FILES += include.xml
EXTRA_DIST_FILES += $(call src-wildcard,*.ly)
EXTRA_DIST_FILES += $(call src-wildcard,*.ily)
EXTRA_DIST_FILES += $(call src-wildcard,*.tely)
EXTRA_DIST_FILES += $(call src-wildcard,*.itely)

XML_FILES = $(filter-out include%,$(call src-wildcard,*.xml))

local-test: $(OUT_FILES)

$(outdir)/collated-files.list: $(OUT_FILES)
	echo $(sort $(OUT_FILES)) > $@

$(outdir)/suffix-texi.texi: suffix-texi.texi $(outdir)/version.itexi $(DOCUMENTATION_LOCALE_TARGET) $(INIT_LY_SOURCES) $(SCHEME_SOURCES)
	LILYPOND_VERSION=$(TOPLEVEL_VERSION) $(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND_BOOK_PROCESS) $(LILYPOND_BOOK_LILYPOND_FLAGS)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) $(LILYPOND_BOOK_FLAGS) --redirect-lilypond-output $<

# Prevent parallel lilypond-book instances for this subdir
.NOTPARALLEL:
