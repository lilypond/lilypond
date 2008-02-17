# ISOLANG must be defined

LANGS = $(shell $(PYTHON) $(buildscript-dir)/langdefs.py)

OUT_ITEXI_FILES = $(ITELY_FILES:%.itely=$(outdir)/%.itexi)

DOCUMENTATION_INCLUDES = \
  -I $(top-src-dir)/Documentation/user \
  -I $(top-build-dir)/Documentation/user/$(outdir)
DOCUMENTATION_INCLUDES += $(foreach lang, $(LANGS), -I $(top-build-dir)/Documentation/$(lang)/user/$(outdir))

LILYPOND_BOOK_INCLUDES += $(DOCUMENTATION_INCLUDES)
MAKEINFO_FLAGS += --force --enable-encoding $(DOCUMENTATION_INCLUDES)
MAKEINFO = LANG= $(MAKEINFO_PROGRAM) $(MAKEINFO_FLAGS)

TEXI2PDF_FLAGS += --batch --tidy
TEXI2PDF_FLAGS += $(DOCUMENTATION_INCLUDES)

TELY_FILES = $(call src-wildcard,*.tely)
DEEP_HTML_FILES = $(TELY_FILES:%.tely=$(outdir)/%/index.html)
PDF_FILES = $(TELY_FILES:%.tely=$(outdir)/%.pdf)
