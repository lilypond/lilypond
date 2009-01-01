depth = ..

TEXINFO_FILES = texinfo.tex $(call src-wildcard,txi-*.tex)
TEX_FILES = $(filter-out $TEXINFO_FILES,$(call src-wildcard,*.tex))
EXTRA_DIST_FILES = $(TEX_FILES) $(TEXINFO_FILES)
STEPMAKE_TEMPLATES = install install-out

INSTALLATION_DIR = $(local_lilypond_datadir)/tex/
INSTALLATION_FILES = $(TEX_FILES)

include $(depth)/make/stepmake.make

local-uninstall:
	-rmdir $(DESTDIR)$(local_lilypond_datadir)/tex

all: $(INSTALLATION_FILES)
