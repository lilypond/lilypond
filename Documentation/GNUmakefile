depth = ..

NAME = documentation
LANGS = $(shell $(PYTHON) $(buildscript-dir)/langdefs.py)
SUBDIRS=user bibliography pictures topdocs misc po $(LANGS)
STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly
LILYPOND_BOOK_FLAGS=--extra-options '-e "(ly:set-option (quote internal-type-checking) \#t)"'
README_TOP_FILES= DEDICATION THANKS 
EXTRA_DIST_FILES= texinfo.css TRANSLATION

include $(depth)/make/stepmake.make

default: local-doc

local-WWW: txt-to-html $(outdir)/index.html

local-WWW-clean: deep-WWW-clean

deep-WWW-clean:
	rm -rf $(outdir)/wiki-dump

OUT_TXT_FILES = $(addprefix $(outdir)/, $(addsuffix .txt, $(README_TOP_FILES)))

# cd $(outdir) rule gets bit hairy for --srcdir configure builds
txt-to-html:
	$(foreach a, $(README_TOP_FILES), cp $(top-src-dir)/$(a) $(outdir)/$(a).txt && ) true
	$(PYTHON) $(step-bindir)/text2html.py $(OUT_TXT_FILES)

po-update:
	make -C po po-update

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
	$(PYTHON) $(buildscript-dir)/texi-langutils.py -d $(outdir) -b "UNTRANSLATED NODE: IGNORE ME" -l $(ISOLANG) -o doc.pot --skeleton --gettext ../user/lilypond.tely
	mv $(outdir)/*.*tely $(ISOLANG)/user
	msgmerge -U po/lilypond-doc.pot $(outdir)/doc.pot
	cp po/lilypond-doc.pot po/$(ISOLANG).po
	@echo "***  Please add a language definition for $(ISOLANG) in buildscripts/langdefs.py  ***"

check-translation:
	find $(ISOLANG)/user/ -name '*.*tely' | xargs $(PYTHON) $(buildscript-dir)/check_translation.py $(buildscript-dir)

skeleton-update:
	$(PYTHON) $(buildscript-dir)/texi-langutils.py -d $(outdir) -b "UNTRANSLATED NODE: IGNORE ME" -l $(ISOLANG) --skeleton ../user/lilypond.tely
	$(PYTHON) $(buildscript-dir)/texi-skeleton-update.py $(ISOLANG)/user $(outdir)
endif
