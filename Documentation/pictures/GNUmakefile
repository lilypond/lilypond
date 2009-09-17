depth = ../..

BITMAP_IMAGES = $(call src-wildcard,*.png) $(call src-wildcard,*.jpg)
EPS_ILLUSTRATIONS = $(call src-wildcard,*.eps)
SVG_FILES = $(call src-wildcard,*.svg)
PDF_DIR = $(call src-wildcard,pdf/*.pdf)

EXTRA_DIST_FILES = $(BITMAP_IMAGES) $(EPS_ILLUSTRATIONS) $(SVG_FILES) \
  $(PDF_DIR)

OUT_PDF_IMAGES = $(EPS_ILLUSTRATIONS:%.eps=$(outdir)/%.pdf)
OUT_BITMAP_IMAGES = $(BITMAP_IMAGES:%=$(outdir)/%)
OUT_BITMAP_IMAGES += $(EPS_ILLUSTRATIONS:%.pdf=$(outdir)/%.png)

STEPMAKE_TEMPLATES = documentation

include $(depth)/make/stepmake.make

###########
### Targets

default:

ifeq ($(out),www)
local-WWW-1: $(OUT_BITMAP_IMAGES) $(OUT_PDF_IMAGES)
endif

#########
### Rules

$(outdir)/%-flat-gray.png: %-flat-gray.png
	convert -depth 8 -geometry 50x50% $< $@

$(outdir)/%-flat-bw.png: %-flat-bw.png
	convert -depth 8 -geometry 50x50% $< $@

$(outdir)/%.png: %.png
	ln -f $< $@

$(outdir)/%.jpg: %.jpg
	ln -f $< $@

$(outdir)/%.png: %.eps
	gs -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -q -sOutputFile=$@ -sDEVICE=png16m -dEPSCrop -dNOPAUSE -f $< -c quit

$(outdir)/%.pdf: %.eps
	gs -dAutoRotatePages=/None -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -sOutputFile=$@ -dEPSCrop -c .setpdfwrite -f $<

