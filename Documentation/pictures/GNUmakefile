depth = ../..

STEPMAKE_TEMPLATES=documentation
XPM_FILES=$(wildcard *.xpm)
OUTGIF_FILES = $(addprefix $(outdir)/,$(XPM_FILES:.xpm=.gif))
OUTPNG_FILES = $(addprefix $(outdir)/,$(XPM_FILES:.xpm=.png))

EXTRA_DIST_FILES= $(XPM_FILES)
OUT_DIST_FILES = $(package-icon)

include $(depth)/make/stepmake.make

default: $(package-icon)
local-dist: $(package-icon)

xgifs: $(OUTGIF_FILES)
pngs: $(OUTPNG_FILES)

local-WWW: $(OUTPNG_FILES)

