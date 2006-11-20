depth = ..

NAME = documentation
LANGS = fr # don't enable unpolished or broken translations
SUBDIRS=user bibliography pictures topdocs misc po $(LANGS)
STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly
LILYPOND_BOOK_FLAGS=--extra-options '-e "(ly:set-option (quote internal-type-checking) \#t)"'
README_TOP_FILES= DEDICATION THANKS 
EXTRA_DIST_FILES= texinfo.css

include $(depth)/make/stepmake.make

default: local-doc

local-WWW: copy-for-me txt-to-html $(outdir)/index.html

local-WWW-clean: deep-WWW-clean

deep-WWW-clean:
	rm -rf $(outdir)/wiki-dump

copy-for-me:
	$(foreach a, $(README_TOP_FILES), cp $(top-src-dir)/$(a) $(outdir)/$(a).txt && ) true

OUT_TXT_FILES = $(addprefix $(outdir)/, $(addsuffix .txt, $(README_TOP_FILES)))

# cd $(outdir) rule gets bit hairy for --srcdir configure builds
txt-to-html:
	$(PYTHON) $(step-bindir)/text2html.py $(OUT_TXT_FILES)

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
	$(PYTHON) $(depth)/buildscripts/texi-langutils.py -d $(outdir) -b "UNTRANSLATED NODE: IGNORE ME" -o doc.pot --skeleton --gettext ../user/lilypond.tely
	mv $(outdir)/*.*tely $(ISOLANG)/user
	msgmerge -U po/lilypond-doc.pot $(outdir)/doc.pot
	cp po/lilypond-doc.pot po/$(ISOLANG).po
endif