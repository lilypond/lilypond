depth = ..

# elispdir defined in config.make

INSTALLATION_DIR=$(elispdir)
INSTALLATION_FILES=$(EL_FILES)

INSTALLATION_OUT_DIR=$(elispdir)
INSTALLATION_OUT_FILES=$(outdir)/lilypond-words.el

STEPMAKE_TEMPLATES=elisp install install-out

include $(depth)/make/stepmake.make 

LILYPOND_WORDS = $(outdir)/lilypond-words.el
LILYPOND_WORDS_DEPENDS =\
  $(topdir)/lily/lily-lexer.cc \
  $(buildscript-dir)/lilypond-words.py \
  $(topdir)/scm/new-markup.scm \
  $(topdir)/ly/engraver-init.ly

$(LILYPOND_WORDS): 
	cd $(topdir) && $(PYTHON) buildscripts/lilypond-words.py --el --dir=$(builddir)/elisp/$(outconfbase)

all: $(LILYPOND_WORDS)
