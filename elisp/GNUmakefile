depth = ..

# elispdir defined in config.make

INSTALLATION_DIR=$(elispdir)
INSTALLATION_FILES=$(EL_FILES)

INSTALLATION_OUT_DIR=$(elispdir)
INSTALLATION_OUT_FILES=$(outdir)/lilypond-words.el


STEPMAKE_TEMPLATES= install install-out

include $(depth)/make/stepmake.make

EL_FILES := $(call src-wildcard,*.el)
SOURCE_FILES += $(EL_FILES)

LILYPOND_WORDS = $(outdir)/lilypond-words.el
LILYPOND_WORDS_DEPENDS =\
  $(top-src-dir)/lily/lily-lexer.cc \
  $(buildscript-dir)/lilypond-words \
  $(top-src-dir)/scm/markup.scm \
  $(top-src-dir)/ly/engraver-init.ly

$(buildscript-dir)/lilypond-words:
	$(MAKE) -C $(depth)/scripts/build

$(LILYPOND_WORDS):
	$(call ly_progress,Making,$@,)
	cd $(top-src-dir) && $(buildscript-dir)/lilypond-words --el --dir=$(top-build-dir)/elisp/$(outconfbase)

all: $(LILYPOND_WORDS)
