depth = ../..

BITMAP_IMAGES = $(call src-wildcard,*.png) $(call src-wildcard,*.jpg)
EPS_ILLUSTRATIONS = $(call src-wildcard,*.eps)
SVG_FILES = $(call src-wildcard,*.svg)
PDF_DIR = $(call src-wildcard,pdf/*.pdf)

OUT_PDF_IMAGES = $(EPS_ILLUSTRATIONS:%.eps=$(outdir)/%.pdf)
OUT_BITMAP_IMAGES = $(BITMAP_IMAGES:%=$(outdir)/%)
OUT_BITMAP_IMAGES += $(EPS_ILLUSTRATIONS:%.eps=$(outdir)/%.png)

STEPMAKE_TEMPLATES =

include $(depth)/make/stepmake.make

###########
### Targets

default:

ifeq ($(out),www)
local-WWW-1: $(OUT_BITMAP_IMAGES) $(OUT_PDF_IMAGES) $(outdir)/pdf
endif

#########
### Rules

$(outdir)/%-flat-gray.png: %-flat-gray.png
	$(call ly_progress,Making,$@,(convert))
	convert -strip -depth 8 -geometry 50x50% $< $@

$(outdir)/%-flat-bw.png: %-flat-bw.png
	$(call ly_progress,Making,$@,(convert))
	convert -strip -depth 8 -geometry 50x50% $< $@

$(outdir)/%.png: $(top-src-dir)/Documentation/pictures/%.png
	$(call ly_progress,Making,$@,(copy))
	cp $< $@

$(outdir)/%.jpg: $(top-src-dir)/Documentation/pictures/%.jpg
	$(call ly_progress,Making,$@,(copy))
	cp $< $@

$(outdir)/%.png: %.eps
	$(call ly_progress,Making,$@,< eps)
	gs -dAutoRotatePages=/None \
           -dPrinted=false \
           -dTextAlphaBits=4 \
           -dGraphicsAlphaBits=4 \
           -q \
           -sOutputFile=$@ \
           -sDEVICE=png16m \
           -dEPSCrop \
           -dNOPAUSE \
           -f $< \
           -c quit

$(outdir)/%.pdf: %.eps
	$(call ly_progress,Making,$@,< eps)
	gs -dAutoRotatePages=/None \
           -dPrinted=false \
           -q \
           -sDEVICE=pdfwrite \
           -dNOPAUSE \
           -dBATCH \
           -sOutputFile=$@ \
           -dEPSCrop \
           -c .setpdfwrite \
           -f $<

$(outdir)/pdf:
	ln -sf $(top-src-dir)/Documentation/pictures/pdf $@
