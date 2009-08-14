depth=../../..
LOCALSTEPMAKE_TEMPLATES=lilypond ly
include $(depth)/make/stepmake.make

LILYPOND_PREVIEW = $(LILYPOND_BINARY) --png -dpreview -dresolution=150

BROKEN = granados.ly #sesto.ly
FINE_LY_FILES = $(filter-out $(BROKEN), $(LY_FILES))
PNG_PAGES = sesto-1 sesto-2

OUT_PNG_FILES = $(FINE_LY_FILES:%.ly=$(outdir)/%.png)
OUT_PNG_PAGES = $(PNG_PAGES:%=$(outdir)/%.png)
OUT_PNG_FILES += $(OUT_PNG_PAGES)

OUT_SMALL_PNG_FILES = $(OUT_PNG_FILES:%.png=%-small.png)

ifeq ($(out),www)
all: $(OUT_PNG_FILES) $(OUT_SMALL_PNG_FILES)
endif

$(outdir)/%.png: %.ly
	$(LILYPOND_PREVIEW) -o $(outdir) $<
	mv $(outdir)/$*.preview.png $@

$(outdir)/%-small.png: $(outdir)/%.png
	pngtopnm $< | pnmscale -w=600 | pnmtopng > $@

$(outdir)/%-1.png: $(outdir)/%.png
	mv $(outdir)/$*-1.preview.png $@

$(outdir)/%-2.png: $(outdir)/%.png
	mv $(outdir)/$*-2.preview.png $@
