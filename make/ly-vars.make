# rules for directories with LilyPond files.

TELY_FILES := $(call src-wildcard,*.tely)
TEXI_FILES_FROM_TELY := $(TELY_FILES:%.tely=$(outdir)/%.texi) $(TEXI_FILES_FROM_TELY)

OMF_FILES += $(foreach format, html pdf, $(foreach f, $(TELY_FILES), $(outdir)/$(f:.tely=.$(format)).omf))

ITELY_FILES := $(call src-wildcard,*.itely)
ITEXI_FILES := $(call src-wildcard,*.itexi)
LY_FILES := $(call src-wildcard,*.ly)
ILY_FILES := $(call src-wildcard,*.ily)

TEXINFO_SOURCES += $(TELY_FILES) $(ITELY_FILES) $(ITEXI_FILES)

EXTRA_DIST_FILES +=$(TELY_FILES) $(LY_FILES) $(ITEXI_FILES) $(ITELY_FILES) $(ILY_FILES)

# prerequisites for all rules invoking compiled lilypond binary
ifeq ($(LILYPOND_EXTERNAL_BINARY),)
INIT_LY_SOURCES = $(wildcard $(top-src-dir)/scm/*.scm)
SCHEME_SOURCES = $(wildcard $(top-src-dir)/ly/*.ly)
endif
