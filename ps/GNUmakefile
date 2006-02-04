depth = ..

PS_FILES = $(call src-wildcard,*.ps)

STEPMAKE_TEMPLATES=install
EXTRA_DIST_FILES = $(PS_FILES)

INSTALLATION_DIR=$(local_lilypond_datadir)/ps/
INSTALLATION_FILES=$(PS_FILES)

# teTeX-3.0 compatibility.
local-install:
	-$(INSTALL) -d $(DESTDIR)$(local_lilypond_datadir)/dvips
	(cd $(DESTDIR)$(local_lilypond_datadir)/dvips && ln -sf ../ps .)

local-uninstall:
	-rm -f $(DESTDIR)$(local_lilypond_datadir)/dvips/ps
	-rmdir $(DESTDIR)$(local_lilypond_datadir)/dvips

include $(depth)/make/stepmake.make
