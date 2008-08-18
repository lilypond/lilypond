# ISOLANG must be defined

LANGS = $(shell $(PYTHON) $(buildscript-dir)/langdefs.py)

DOCUMENTATION_INCLUDES = \
  -I $(top-src-dir)/Documentation/user \
  -I $(top-build-dir)/Documentation/user/$(outdir)

LILYPOND_BOOK_INCLUDES += $(DOCUMENTATION_INCLUDES)
MAKEINFO_FLAGS += --force --enable-encoding $(DOCUMENTATION_INCLUDES)
MAKEINFO = LANG= $(MAKEINFO_PROGRAM) $(MAKEINFO_FLAGS)

TEXI2PDF_FLAGS += -q --batch $(DOCUMENTATION_INCLUDES)

TELY_FILES = $(call src-wildcard,*.tely)
OUT_TEXI_FILES = $(TELY_FILES:%.tely=$(outdir)/%.texi)
DEEP_HTML_FILES = $(TELY_FILES:%.tely=$(outdir)/%/index.html)
PDF_FILES = $(TELY_FILES:%.tely=$(outdir)/%.pdf)

ITELY_FILES := $(call src-wildcard,*.itely)
ITEXI_FILES := $(call src-wildcard,*.itexi)
