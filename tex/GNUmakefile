depth = ..

TEX_FILES = $(wildcard *.tex) $(outdir)/music-drawing-routines.ps

EXTRA_DIST_FILES = $(TEX_FILES)
STEPMAKE_TEMPLATES=install

INSTALLATION_DIR=$(local_lilypond_datadir)/tex/
INSTALLATION_FILES=$(TEX_FILES)

include $(depth)/make/stepmake.make

$(outdir)/music-drawing-routines.ps: $(depth)/ps/music-drawing-routines.ps
	-cp $< $@

all: $(INSTALLATION_FILES)
