# rules for directories with LilyPond files.

TELY_FILES := $(call src-wildcard,*.tely)
TEXI_FILES_FROM_TELY := $(TELY_FILES:%.tely=$(outdir)/%.texi) $(TEXI_FILES_FROM_TELY)

ITELY_FILES := $(call src-wildcard,*.itely)
ITEXI_FILES := $(call src-wildcard,*.itexi)
LY_FILES := $(call src-wildcard,*.ly)
ILY_FILES := $(call src-wildcard,*.ily)

LILYPOND_BOOK_INCLUDES = -I $(abs-src-dir)

# prerequisites for all rules invoking compiled lilypond binary
ifeq ($(LILYPOND_EXTERNAL_BINARY),)
INIT_LY_SOURCES = $(wildcard $(top-src-dir)/ly/*.ly)
SCHEME_SOURCES = $(wildcard $(top-src-dir)/scm/*.scm)
endif
