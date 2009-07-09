depth=../..

LATEX_FILES =$(call src-wildcard,*.latex)


EXTRA_DIST_FILES = $(LATEX_FILES) $(IMAGES) $(EPS_ILLUSTRATIONS)

IMAGES=$(call src-wildcard,*.png)
EPS_ILLUSTRATIONS=
PDF_ILLUSTRATIONS=

OUT_PDF_IMAGES=$(IMAGES:%.png=$(outdir)/%.pdf) $(addprefix $(outdir)/,$(PDF_ILLUSTRATIONS))

OUT_PNG_IMAGES=$(OUT_PDF_IMAGES:%.pdf=%.png)

#OUT_MASTER_TEXI_FILES = $(outdir)/lilypond-internals.texi
OUT_TEXI_FILES=$(ITEXI_FILES:%.itexi=$(outdir)/%.texi)\
 $(ITELY_FILES:%.itely=$(outdir)/%.texi)

HTML_FILES = $(TELY_FILES:%.tely=$(outdir)/%-big-page.html)\

# $(outdir)/lilypond-internals-big-page.html

# todo: add latex.
PDF_FILES = $(TELY_FILES:%.tely=$(outdir)/%.pdf)\

# $(outdir)/lilypond-internals.pdf

MAIN_INFO_DOC = lilypond-essay
INFO_DOCS = lilypond-essay
INFO_FILES = $(INFO_DOCS:%=$(outdir)/%.info)

TEXI2PDF_FLAGS = \
  -I $(outdir) \
  -I $(top-src-dir)/Documentation/essay \
  -I $(top-build-dir)/Documentation/essay/$(outconfbase)

ifeq ($(out),www)
INFO_IMAGES_DIR = lilypond
DEST_INFO_IMAGES_SUBDIR = Documentation/essay
endif

STEPMAKE_TEMPLATES=texinfo documentation
#OMF_FILES += $(outdir)/lilypond-internals.html.omf

LOCALSTEPMAKE_TEMPLATES=lilypond ly

include $(depth)/make/stepmake.make

default:

info: $(INFO_FILES)
	@echo export LILYPOND_DATADIR=$(LILYPOND_DATADIR)
	@echo export PYTHONPATH=$(PYTHONPATH)

xml: $(outdir)/lilypond-essay/lilypond.xml
#xml: $(outdir)/lilypond/lilypond.xml $(outdir)/lilypond-internals/lilypond-internals.xml


local-clean:
	rm -f $(INFO_IMAGES_DIR)

local-help: extra-local-help

extra-local-help:
	@echo -e "\
  info        update info pages\n\
  xml	      update Docbook xml documentation\n\
"

#
# Split manuals in HTML
#
$(outdir)/lilypond-essay/index.html: $(OUT_PNG_IMAGES) $(OUT_EPS_IMAGES)
#$(outdir)/lilypond-learning/index.html: $(OUT_PNG_IMAGES) $(OUT_EPS_IMAGES)


#
# Manuals in one big HTML page
#
$(outdir)/lilypond-essay-big-page.html: $(OUT_PNG_IMAGES)
#$(outdir)/lilypond-learning-big-page.html: $(OUT_PNG_IMAGES)

$(outdir)/lilypond.xml: $(outdir)/lilypond-essay.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$@ --docbook $<

#$(outdir)/lilypond-internals/lilypond-internals.xml: $(outdir)/lilypond-internals.texi
#	mkdir -p $(dir $@)
#	$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$(outdir)/lilypond-internals --docbook $<

$(outdir)/lilypond-essay.pdf: $(OUT_PDF_IMAGES)

$(outdir)/%.png: %.png
	convert -depth 8 -geometry 50x50% $< $@

$(outdir)/%.png: %.eps
	gs -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -q -sOutputFile=$@ -sDEVICE=png16m -dEPSCrop -dNOPAUSE -f $< -c quit

$(outdir)/%.pdf: %.png
	convert -depth 8 $< $@

$(outdir)/%.pdf: %.eps
	gs -dAutoRotatePages=/None -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -sOutputFile=$@ -dEPSCrop -c .setpdfwrite -f $<


DEEP_HTML_FILES =\
 $(outdir)/lilypond-essay/index.html\

# $(outdir)/lilypond/index.html\
# $(outdir)/lilypond-internals/index.html\
# $(outdir)/music-glossary/index.html\
# $(outdir)/lilypond-program/index.html\
# $(outdir)/lilypond-learning/index.html

# Symlinks to refer to external source documents from split and non-split HTML
source-links = $(outdir)/source $(outdir)/lilypond/source $(outdir)/music-glossary/source \
 $(outdir)/lilypond-program/source $(outdir)/lilypond-learning/source $(outdir)/lilypond-internals/source


$(outdir)/source:
	@rm -f $(@)
	ln -sf $(depth) $(@)

$(outdir)/%/source:
	@rm -f $(@)
	mkdir -p $(dir $@)
	ln -sf $(depth)/.. $(@)

ifeq ($(out),www)
local-WWW-1: $(MASTER_TEXI_FILES) $(PDF_FILES) info

local-WWW-2: $(HTML_FILES) $(DEEP_HTML_FILES) $(source-links)
endif

$(outdir)/%.bib: %.bib
	ln -f $< $@

# lilypond.texi deps
$(top-build-dir)/mf/$(outconfbase)/feta16list.ly:
	$(MAKE) -C $(top-src-dir)/mf

$(outdir)/lilypond-essay.texi: $(ITELY_FILES) $(ITEXI_FILES)

.SECONDARY:
