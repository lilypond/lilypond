depth = ../..

STEPMAKE_TEMPLATES=documentation
XPM_FILES=$(wildcard *.xpm)
OUTGIF_FILES = $(addprefix $(outdir)/,$(XPM_FILES:.xpm=.gif))
OUTPNG_FILES = $(addprefix $(outdir)/,$(XPM_FILES:.xpm=.png))

EXTRA_DIST_FILES= $(XPM_FILES)
OUT_DIST_FILES = $(icon) #$(package-icon)

icon = $(outdir)/lilypond.ico
ICON_SIZES=48 32 16

#$(outdir)/lilypond.ico: platte-lucht-kikker-ly-48.xpm

include $(depth)/make/stepmake.make

default: $(icon)
local-dist: $(icon) # $(package-icon)

$(outdir)/%.ico: $(ICON_SIZES:%=$(outdir)/\%-%.png) $(ICON_SIZES:%=$(outdir)/\%-%-8.png)
	#convert +adjoin $^ $@
	icotool --output=$@ --create $^

# cancel default rule that breaks transparency
$(outdir)/%.png: %.xpm

$(outdir)/%-48.png: %-48.xpm
	convert -sample 48 -depth 24 $< $@
		   
$(outdir)/%-32.png: %-48.xpm
	convert -depth 24 -sample 32 $< $@

$(outdir)/%-16.png: %-48.xpm
	convert -depth 24 -sample 16 $< $@

$(outdir)/%-8.png: $(outdir)/%.png
	convert -depth 8 $< $@

xgifs: $(OUTGIF_FILES)
pngs: $(OUTPNG_FILES)

local-WWW: $(OUTPNG_FILES)

