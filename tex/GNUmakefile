depth = ..

TEX_FILES = $(wildcard *.tex)  texinfo.cnf
EXTRA_DIST_FILES = $(TEX_FILES) 
STEPMAKE_TEMPLATES=install install-out

INSTALLATION_DIR=$(local_lilypond_datadir)/tex/
INSTALLATION_FILES=$(TEX_FILES) $(outdir)/music-drawing-routines.ps

INSTALLATION_OUT_SUFFIXES=1
INSTALLATION_OUT_DIR1=$(local_lilypond_datadir)/fonts/enc
INSTALLATION_OUT_FILES1=$(outdir)/latin1.enc

include $(depth)/make/stepmake.make

$(outdir)/music-drawing-routines.ps: $(depth)/ps/music-drawing-routines.ps
	rm -f $@
	-ln $< $@

$(outdir)/latin1.enc: GNUmakefile
	echo '/ISOLatin1Encoding' > $@.orig
	echo 'ISOLatin1Encoding pstack' \
	  | gs -sDEVICE=nullpage  -quiet - >> $@.orig 2>&1
	echo 'def' >> $@.orig
	sed 's|/minus|/hyphen|' < $@.orig > $@

# teTeX-2.0.x compatibility.
local-install:
	-$(INSTALL) -d $(DESTDIR)$(local_lilypond_datadir)/tex
	(cd $(DESTDIR)$(local_lilypond_datadir)/tex && ln -sf ../fonts/enc .)

local-uninstall:
	-rm -f $(DESTDIR)$(local_lilypond_datadir)/tex/enc
	-rmdir $(DESTDIR)$(local_lilypond_datadir)/tex

all: $(INSTALLATION_FILES) $(INSTALLATION_OUT_FILES1)

