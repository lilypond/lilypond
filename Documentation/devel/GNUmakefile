depth = ../..

XREF_MAP_FLAGS = --split section -I $(abs-src-dir)

STEPMAKE_TEMPLATES = documentation tex texinfo topdocs
LOCALSTEPMAKE_TEMPLATES = ly

include $(depth)/make/stepmake.make

HTML_PAGE_NAMES = index
OUT_HTML_FILES = $(HTML_PAGE_NAMES:%=$(outdir)/%.html)


MASTER_TEXI_FILES = contrib-guide.texi
PDF_FILES = $(MASTER_TEXI_FILES:%.texi=$(outdir)/%.pdf)
HTML_FILES = $(MASTER_TEXI_FILES:%.texi=$(outdir)/%/index.html) \
  $(MASTER_TEXI_FILES:%.texi=$(outdir)/%-big-page.html)
source-links = $(outdir)/source $(MASTER_TEXI_FILES:%.texi=$(outdir)/%/source)

default: local-doc

$(outdir)/source:
	@rm -f $(@)
	ln -sf $(depth) $(@)

$(outdir)/%/source:
	@rm -f $(@)
	mkdir -p $(dir $@)
	ln -sf $(depth)/.. $(@)

ifeq ($(out),www)
local-WWW-1: $(PDF_FILES)

local-WWW-2: $(HTML_FILES) $(OUT_HTML_FILES) $(source-links)
endif

$(PDF_FILES) $(HTML_FILES): $(ITEXI_FILES)
