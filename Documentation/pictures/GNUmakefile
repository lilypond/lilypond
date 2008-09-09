depth = ../..

STEPMAKE_TEMPLATES=documentation
XPM_FILES=$(call src-wildcard,*.xpm)
EXTRA_DIST_FILES= $(XPM_FILES)

lilypond-icon = $(outdir)/lilypond.ico
ly-icon = $(outdir)/ly.ico

include $(depth)/make/stepmake.make

ifeq ($(PLATFORM_WINDOWS),yes)

$(outdir)/%.ico: %.xpm
	$(PYTHON) $(buildscript-dir)/genicon.py $< $@

default: $(lilypond-icon) $(ly-icon)

endif # PLATFORM_WINDOWS

local-dist: $(icon) 
xgifs: $(OUTGIF_FILES)
pngs: $(OUTPNG_FILES)
local-WWW-1: $(OUTPNG_FILES)
