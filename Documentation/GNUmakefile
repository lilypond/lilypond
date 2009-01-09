depth = ..

NAME = documentation
LANGS = $(shell $(PYTHON) $(top-src-dir)/python/langdefs.py)
SUBDIRS=devel user bibliography pictures topdocs misc po $(LANGS)
STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly
LILYPOND_BOOK_FLAGS=--extra-options '-e "(ly:set-option (quote internal-type-checking) \#t)"'
README_TOP_FILES= DEDICATION THANKS
EXTRA_DIST_FILES= TRANSLATION

include $(depth)/make/stepmake.make

HTML_PAGE_NAMES= index translations
OUT_HTML_FILES= $(HTML_PAGE_NAMES:%=$(outdir)/%.html)
OUT_CSS_FILES= $(CSS_FILES:%.css=$(outdir)/%.css)

default: local-doc

local-WWW-2: txt-to-html $(OUT_HTML_FILES)

local-WWW-clean: deep-WWW-clean

deep-WWW-clean:
	rm -rf $(outdir)/wiki-dump

OUT_TXT_FILES = $(addprefix $(outdir)/, $(addsuffix .txt, $(README_TOP_FILES)))

# cd $(outdir) rule gets bit hairy for --srcdir configure builds
txt-to-html:
	$(foreach a, $(README_TOP_FILES), cp $(top-src-dir)/$(a) $(outdir)/$(a).txt && ) true
	$(PYTHON) $(step-bindir)/text2html.py $(OUT_TXT_FILES)

$(OUT_HTML_FILES): $(OUT_CSS_FILES)

$(outdir)/%.css: %.css
	ln -f $< $@


### Translations maintenance targets

po-update:
	make -C po po-update

all-translations-update: po-update translation-status
	$(foreach lang, $(LANGS), make ISOLANG=$(lang) skeleton-update snippet-update &&) true

ifneq ($(NO_COLOR),)
CHECK_TRANSLATION_FLAGS = --no-color
endif

ifneq ($(ISOLANG),)
new-lang:
	@if (( $$(file -b $(ISOLANG)) == directory )) 2>/dev/null; \
	then echo "Error: $(ISOLANG) directory already exists. Exiting." ; \
	exit 3 ; \
	fi
	mkdir -p $(ISOLANG)/user
	cp fr/GNUmakefile $(ISOLANG)
	cp fr/user/GNUmakefile $(ISOLANG)/user
	sed -i -e 's/ISOLANG *= *fr/ISOLANG = $(ISOLANG)/' $(ISOLANG)/GNUmakefile $(ISOLANG)/user/GNUmakefile
	$(auxscript-dir)/texi-langutils.py -d $(outdir) -l $(ISOLANG) -o doc.pot --skeleton --gettext ../user/lilypond-learning.tely
	mv $(outdir)/*.*tely $(ISOLANG)/user
	msgmerge -U po/lilypond-doc.pot $(outdir)/doc.pot
	cp po/lilypond-doc.pot po/$(ISOLANG).po
	@echo "***  Please add a language definition for $(ISOLANG) in python/langdefs.py  ***"

CHECKED_FILES = $(ISOLANG)/index.html.in $(shell find $(ISOLANG)/user/ -maxdepth 1 -name '*.*te??')

TELY_FILES = $(call src-wildcard,$(ISOLANG)/user/*.tely)
skeleton-update:
	$(auxscript-dir)/texi-langutils.py -d $(outdir) -l $(ISOLANG) --skeleton $(TELY_FILES:$(ISOLANG)/user/%.tely=../user/%.tely)
	$(auxscript-dir)/texi-skeleton-update.py $(ISOLANG)/user $(outdir)

snippet-update:
	$(auxscript-dir)/update-snippets.py user $(ISOLANG)/user '*.itely'

DOCUMENTS_INCLUDES:=-I $(ISOLANG)/user \
-I $(top-build-dir)/Documentation/$(ISOLANG)/user/out-www \
-I $(top-src-dir)/input/lsr \
-I $(top-build-dir)/input/lsr/out-www \
-I user -I $(top-build-dir)/Documentation/user/out-www

else # ISOLANG is empty

DOCUMENTS_INCLUDES:=-I user \
-I $(top-build-dir)/Documentation/user/out-www \
-I $(top-src-dir)/input/lsr \
-I $(top-build-dir)/input/lsr/out-www

endif # ISOLANG

check-xrefs:
	$(auxscript-dir)/check_texi_refs.py --batch \
	$(DOCUMENTS_INCLUDES) $(auxpython-dir)/manuals_definitions.py

fix-xrefs:
	$(auxscript-dir)/check_texi_refs.py --auto-fix \
	$(DOCUMENTS_INCLUDES) $(auxpython-dir)/manuals_definitions.py

check-translation:
	$(auxscript-dir)/check_translation.py $(CHECK_TRANSLATION_FLAGS) $(CHECKED_FILES)

update-translation:
	$(auxscript-dir)/check_translation.py --update $(CHECK_TRANSLATION_FLAGS) $(CHECKED_FILES)

translation-status:
	make -C po out=www messages
	$(auxscript-dir)/translations-status.py

local-help: extra-local-help

extra-local-help:
	@echo -e "\
  check-xrefs [ISOLANG=LL] validate Texinfo cross-references\n\
  fix-xrefs [ISOLANG=LL]   interactively fix Texinfo cross-references (use with caution)\n\
\n\
Translations specific targets (see TRANSLATION for details):\n\
  new-lang ISOLANG=LL  create and initialize subdirectory \'LL' for language \'LL\n\
  po-update            update and replace PO files for documentation with msgmerged versions\n\
  check-translation ISOLANG=LL   show changes in English docs since last translation update\n\
  update-translation ISOLANG=LL  call $$$$EDITOR to help updating the translation\n\
  skeleton-update ISOLANG=LL     update Texinfo skeleton files\n\
  snippet-update ISOLANG=LL      update ly snippets in Texinfo translation according to\n\
                              docs in English (use with caution)\n\
\n\
LL refers to the desired locale (most often only the ISO 639 language code).\n"

info:
	$(MAKE) -C user info
