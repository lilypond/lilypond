depth = ../..

STEPMAKE_TEMPLATES=documentation
XPM_FILES=$(wildcard *.xpm)
OUTGIF_FILES = $(addprefix $(outdir)/,$(XPM_FILES:.xpm=.gif))
OUTPNG_FILES = $(addprefix $(outdir)/,$(XPM_FILES:.xpm=.png))

EXTRA_DIST_FILES= $(XPM_FILES)

include $(depth)/make/stepmake.make

ifeq ($(PLATFORM_WINDOWS),yes)
OUT_DIST_FILES = $(lilypond-icon) $(ly-icon) # $(package-icon)

ICON_SIZES=48 32 16

lilypond-icon = $(outdir)/lilypond.ico
ly-icon = $(outdir)/ly.ico

#$(outdir)/lilypond.ico: platte-lucht-kikker-ly-48.xpm

$(outdir)/%.ico: $(ICON_SIZES:%=$(outdir)/\%-%.png) $(ICON_SIZES:%=$(outdir)/\%-%-8.png)
	#convert +adjoin $^ $@
	icotool --output=$@ --create $^

# cancel default rule that breaks transparency
$(outdir)/%.png: %.xpm

$(outdir)/%-48.png: %.xpm
	convert -sample 48 -depth 24 $< $@
		
$(outdir)/%-32.png: %.xpm
	convert -depth 24 -sample 32 $< $@

$(outdir)/%-16.png: %.xpm
	convert -depth 24 -sample 16 $< $@

$(outdir)/%-8.png: $(outdir)/%.png
	convert -depth 8 $< $@
endif # PLATFORM_WINDOWS

default: $(lilypond-icon) $(ly-icon)
local-dist: $(icon) # $(package-icon)
xgifs: $(OUTGIF_FILES)
pngs: $(OUTPNG_FILES)
local-WWW: $(OUTPNG_FILES)

