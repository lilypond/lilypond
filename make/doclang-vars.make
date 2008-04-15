# ISOLANG must be defined

LANGS = $(shell $(PYTHON) $(buildscript-dir)/langdefs.py)

SOURCE_PNG_IMAGES=$(shell ls $(top-src-dir)/Documentation/user/*.png)
OUT_PNG_IMAGES=$(SOURCE_PNG_IMAGES:$(top-src-dir)/Documentation/user/%.png=$(outdir)/%.png) $(outdir)/context-example.png

DOCUMENTATION_INCLUDES = \
  -I $(top-src-dir)/Documentation/user \
  -I $(top-build-dir)/Documentation/user/$(outdir)

LILYPOND_BOOK_INCLUDES += $(DOCUMENTATION_INCLUDES)
MAKEINFO_FLAGS += --force --enable-encoding -D 'version $(TOPLEVEL_VERSION)' $(DOCUMENTATION_INCLUDES)
MAKEINFO = LANG= $(MAKEINFO_PROGRAM) $(MAKEINFO_FLAGS)

TEXI2HTML_INIT= --init-file=$(top-src-dir)/lilypond-texi2html.init
TEXI2HTML_LANG=--lang=$(ISOLANG)
TEXI2HTML_FLAGS += $(TEXI2HTML_LANG)
TEXI2HTML = LANG= $(TEXI2HTML_PROGRAM)

TEXI2PDF_FLAGS += --batch --command '@set version $(TOPLEVEL_VERSION)'
TEXI2PDF_FLAGS += $(DOCUMENTATION_INCLUDES)

TELY_FILES = $(call src-wildcard,*.tely)
OUT_TEXI_FILES = $(TELY_FILES:%.tely=$(outdir)/%.texi)
DEEP_HTML_FILES = $(TELY_FILES:%.tely=$(outdir)/%/index.html)
PDF_FILES = $(TELY_FILES:%.tely=$(outdir)/%.pdf)

ITELY_FILES := $(call src-wildcard,*.itely)
ITEXI_FILES := $(call src-wildcard,*.itexi)
