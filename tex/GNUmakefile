depth = ..

TEX_FILES = $(wildcard *.tex)  texinfo.cnf
EXTRA_DIST_FILES = $(TEX_FILES) 
STEPMAKE_TEMPLATES=install

INSTALLATION_DIR=$(local_lilypond_datadir)/tex/
INSTALLATION_FILES=$(TEX_FILES) $(outdir)/latin1.enc  $(outdir)/music-drawing-routines.ps

include $(depth)/make/stepmake.make

$(outdir)/music-drawing-routines.ps: $(depth)/ps/music-drawing-routines.ps
	rm -f $@
	-ln $< $@

$(outdir)/latin1.enc: GNUmakefile
	echo '/ISOLatin1Encoding ' > $@ 
	echo 'ISOLatin1Encoding pstack' | gs -sDEVICE=nullpage  -quiet -  >> $@
	echo ' def ' >> $@

all: $(INSTALLATION_FILES)
