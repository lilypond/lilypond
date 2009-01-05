depth = ..

# elispdir defined in config.make

INSTALLATION_DIR=$(elispdir)
INSTALLATION_FILES=$(EL_FILES)

INSTALLATION_OUT_DIR=$(elispdir)
INSTALLATION_OUT_FILES=$(outdir)/lilypond-words.el

STEPMAKE_TEMPLATES=elisp install install-out
EXTRA_DIST_FILES= emacsclient.patch server.el.patch 

include $(depth)/make/stepmake.make

LILYPOND_WORDS = $(outdir)/lilypond-words.el
LILYPOND_WORDS_DEPENDS =\
  $(top-src-dir)/lily/lily-lexer.cc \
  $(buildscript-dir)/lilypond-words \
  $(top-src-dir)/scm/markup.scm \
  $(top-src-dir)/ly/engraver-init.ly

$(buildscript-dir)/lilypond-words:
	make -C $(depth)/scripts/build

$(LILYPOND_WORDS):
	cd $(top-src-dir) && $(buildscript-dir)/lilypond-words --el --dir=$(top-build-dir)/elisp/$(outconfbase)

all: $(LILYPOND_WORDS)
