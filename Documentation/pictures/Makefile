depth = ../..

include $(depth)/make/Stepmake.make
include $(stepdir)/Documentation_rules.make

XPM_FILES=$(wildcard *.xpm)
OUTGIF_FILES = $(addprefix $(outdir)/,$(XPM_FILES:.xpm=.gif))

EXTRA_DIST_FILES= $(XPM_FILES)
gifs: $(OUTGIF_FILES)

local-WWW: $(OUTGIF_FILES)

icon: $(outdir)/lelie_icon.gif
	cp $< $(depth)/$(outdir)/$(package)-icon.gif
