HTML_PAGE_NAMES = translations
HTML_FILES = $(HTML_PAGE_NAMES:%=%.html)
OUT_HTML_FILES = $(HTML_PAGE_NAMES:%=$(outdir)/%.html)

# ISOLANG must be defined

LANGS = $(shell $(PYTHON) $(buildscript-dir)/langdefs.py)

CSS_SOURCE_FILES = $(shell ls $(top-src-dir)/Documentation/lilypond*.css)

TELY_FILES := $(call src-wildcard,*.tely)
TEXI_FILES := $(call src-wildcard,*.texi)
MASTER_TEXI_FILES := $(TEXI_FILES) $(TELY_FILES:%.tely=$(outdir)/%.texi)

TEXINFO_MANUALS =\
 $(TELY_FILES:%.tely=%)\
 $(TEXI_FILES:%.texi=%)

SPLITTED_HTML_MANUALS = $(foreach manual, $(TEXINFO_MANUALS),\
 $(if $(findstring $(manual), $(UNSPLITTED_HTML_MANUALS)),,$(manual)))

OUT_HTML_FILES += $(UNSPLITTED_HTML_MANUALS:%=$(top-build-dir)/Documentation/$(outdir)/%.$(ISOLANG).html)
BIG_PAGE_HTML_FILES := $(SPLITTED_HTML_MANUALS:%=$(top-build-dir)/Documentation/$(outdir)/%-big-page.$(ISOLANG).html)
DEEP_HTML_FILES := $(SPLITTED_HTML_MANUALS:%=$(top-build-dir)/Documentation/$(outdir)/%/index.$(ISOLANG).html)
PDF_FILES := $(TELY_FILES:%.tely=$(top-build-dir)/Documentation/$(outdir)/%.$(ISOLANG).pdf)

ITELY_FILES := $(call src-wildcard,*.itely)
ITEXI_FILES := $(call src-wildcard,*.itexi)

DOCUMENTATION_INCLUDES = \
  -I $(top-src-dir)/Documentation \
  -I $(top-build-dir)/Documentation/$(outdir)

LILYPOND_BOOK_INCLUDES += $(DOCUMENTATION_INCLUDES)
MAKEINFO_FLAGS += --force --enable-encoding $(DOCUMENTATION_INCLUDES)
MAKEINFO = LANG= $(MAKEINFO_PROGRAM) $(MAKEINFO_FLAGS)

# texi2html xref map files
# FIXME: duplicated in stepake/texinfo-vars.make make/doc-i18n-root-vars.make
XREF_MAPS_DIR=$(top-build-dir)/$(outdir)/xref-maps
XREF_MAPS_FILES=$(TEXINFO_MANUALS:%=$(XREF_MAPS_DIR)/%.$(ISOLANG).xref-map)
XREF_MAP_FLAGS += -I $(outdir)

WEB_MANUALS=general

###########
ifneq ($(ISOLANG),)
TEXI2HTML_LANG = --lang=$(ISOLANG)
endif

DOC_TEXI2HTML_INIT = --init-file=$(top-src-dir)/Documentation/lilypond-texi2html.init
WEB_TEXI2HTML_INIT =--init-file=$(top-src-dir)/Documentation/web-texi2html.init
TEXI2HTML_INIT = $(DOC_TEXI2HTML_INIT)

DOC_TEXI2HTML_SPLIT = --prefix=index --split=section
WEB_TEXI2HTML_SPLIT = --prefix=index --split=node --node-files
TEXI2HTML_SPLIT = $(DOC_TEXI2HTML_SPLIT)

$(top-build-dir)/Documentation/$(outdir)/index.$(ISOLANG).html:\
	TEXI2HTML_INIT = $(WEB_TEXI2HTML_INIT)
$(top-build-dir)/Documentation/$(outdir)/index.$(ISOLANG).html:\
	TEXI2HTML_SPLIT := $(WEB_TEXI2HTML_SPLIT)

TEXI2HTML_INCLUDES += --I=. --I=$(src-dir) --I=$(outdir) $(DOCUMENTATION_INCLUDES) --I=$(XREF_MAPS_DIR)
TEXI2HTML_FLAGS += $(TEXI2HTML_INCLUDES) $(TEXI2HTML_INIT) $(TEXI2HTML_LANG)
TEXI2HTML = SRC_DIR=$(src-dir) PERL_UNICODE=SD $(TEXI2HTML_PROGRAM)
###########

TEXI2PDF_FLAGS += --batch $(DOCUMENTATION_INCLUDES)
TEXI2PDF_FLAGS += -I $(LYS_OUTPUT_DIR)

ifdef QUIET_BUILD
TEXI2PDF_FLAGS += -q
endif

DOCUMENTATION_LOCALE_TARGET = $(outdir)/doc-po
TRANSLATION_LILY_IMAGES = $(outdir)/translation-lily-images
