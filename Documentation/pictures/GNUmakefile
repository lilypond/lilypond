depth = ../..

STEPMAKE_TEMPLATES=documentation
XPM_FILES=$(wildcard *.xpm)
OUTGIF_FILES = $(addprefix $(outdir)/,$(XPM_FILES:.xpm=.gif))
OUTPNG_FILES = $(addprefix $(outdir)/,$(XPM_FILES:.xpm=.png))

EXTRA_DIST_FILES= $(XPM_FILES)

include $(depth)/make/stepmake.make

gifs: $(OUTGIF_FILES)
pngs: $(OUTPNG_FILES)

# local-WWW: $(OUTGIF_FILES)
local-WWW: $(OUTPNG_FILES)

icon: $(outdir)/lelie_icon.gif
	cp $< $(depth)/$(outdir)/$(package)-icon.gif
