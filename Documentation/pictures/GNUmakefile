depth = ../..


BITMAP_IMAGES = $(call src-wildcard,*.png) $(call src-wildcard,*.jpg)
EPS_ILLUSTRATIONS = $(call src-wildcard,*.eps)
SVG_FILES = $(call src-wildcard,*.svg)

EXTRA_DIST_FILES = $(BITMAP_IMAGES) $(EPS_ILLUSTRATIONS) $(SVG_FILES)

OUT_PDF_IMAGES = $(EPS_ILLUSTRATIONS:%.eps=$(outdir)/%.pdf)
OUT_BITMAP_IMAGES = $(BITMAP_IMAGES:%=$(outdir)/%)
OUT_BITMAP_IMAGES += $(EPS_ILLUSTRATIONS:%.pdf=$(outdir)/%.png)
OUT_SVG_IMAGES = $(SVG_FILES:%.svg=$(outdir)/%.png)

STEPMAKE_TEMPLATES = documentation

include $(depth)/make/stepmake.make

###########
### Targets

default:

ifeq ($(out),www)
local-WWW-1: $(OUT_BITMAP_IMAGES) $(OUT_PDF_IMAGES) $(OUT_SVG_IMAGES)
endif

#########
### Rules

$(outdir)/%-flat-gray.png: %-flat-gray.png
	convert -depth 8 -geometry 50x50% $< $@

$(outdir)/%-flat-bw.png: %-flat-bw.png
	convert -depth 8 -geometry 50x50% $< $@

$(outdir)/%.png: %.svg
	convert -depth 8 -alpha Off -background white -layers flatten -trim +repage $< $@

$(outdir)/%.png: %.png
	ln -f $< $@

$(outdir)/%.jpg: %.jpg
	ln -f $< $@

$(outdir)/%.png: %.eps
	gs -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -q -sOutputFile=$@ -sDEVICE=png16m -dEPSCrop -dNOPAUSE -f $< -c quit

$(outdir)/%.pdf: %.eps
	gs -dAutoRotatePages=/None -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -sOutputFile=$@ -dEPSCrop -c .setpdfwrite -f $<

