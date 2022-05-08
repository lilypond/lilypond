depth = ..

TEMPLATES = install install-out

include $(depth)/make/lilypond.make

EL_FILES := $(call src-wildcard,*.el)
LILYPOND_WORDS = $(outdir)/lilypond-words.el

# elispdir defined in config.make
INSTALLATION_DIR=$(elispdir)
INSTALLATION_FILES=$(EL_FILES)

INSTALLATION_OUT_DIR=$(elispdir)
INSTALLATION_OUT_FILES=$(LILYPOND_WORDS)

$(LILYPOND_WORDS):
	$(call ly_progress,Making,$@,)
	cd $(top-src-dir) && $(PYTHON) $(buildscript-dir)/lilypond-words.py --el --dir=$(top-build-dir)/elisp/$(outdir)

all: $(LILYPOND_WORDS)
