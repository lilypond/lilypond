depth = ..

PS_FILES = $(wildcard *.ps)

STEPMAKE_TEMPLATES=install
EXTRA_DIST_FILES = $(PS_FILES) 

INSTALLATION_DIR=$(local_lilypond_datadir)/ps/
INSTALLATION_FILES=$(PS_FILES)

# teTeX-3.0 compatibility.
local-install:
	-$(INSTALL) -d $(local_lilypond_datadir)/dvips
	(cd $(local_lilypond_datadir)/dvips && ln -sf ../ps .)

local-uninstall:
	-rm -f $(local_lilypond_datadir)/dvips/ps
	-rmdir $(local_lilypond_datadir)/dvips

include $(depth)/make/stepmake.make
