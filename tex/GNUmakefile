depth = ..

TEX_FILES = $(filter-out texinfo.tex, $(call src-wildcard,*.tex))
EXTRA_DIST_FILES = $(TEX_FILES) texinfo.tex
STEPMAKE_TEMPLATES = install install-out

INSTALLATION_DIR = $(local_lilypond_datadir)/tex/
INSTALLATION_FILES = $(TEX_FILES)

include $(depth)/make/stepmake.make

local-uninstall:
	-rmdir $(DESTDIR)$(local_lilypond_datadir)/tex

all: $(INSTALLATION_FILES)
