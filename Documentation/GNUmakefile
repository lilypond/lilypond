depth = ..

########################
#  Documentation build #
########################

#############
### Variables

NAME = documentation
LANGS = $(shell $(PYTHON) $(top-src-dir)/python/langdefs.py)
MANUALS_SUBDIRS = application contributor essay learning notation
SUBDIRS = $(MANUALS_SUBDIRS) snippets bibliography logo pictures misc po $(LANGS)
STEPMAKE_TEMPLATES = documentation texinfo tex omf
LOCALSTEPMAKE_TEMPLATES = lilypond ly

### Extra flags

LILYPOND_BOOK_FLAGS = --extra-options '-e "(ly:set-option (quote internal-type-checking) \#t)"'
TEXI2PDF_FLAGS = \
  -I $(outdir) \
  -I $(top-build-dir)/Documentation/$(outconfbase)
$(outdir)/snippets-big-page.html: TEXI2HTML_FLAGS += -D short_toc
$(outdir)/snippets/index..html: TEXI2HTML_FLAGS += -D short_toc

### Extra source files

README_TOP_FILES= DEDICATION THANKS
LATEX_FILES =$(call src-wildcard,*.latex)
IMAGES=$(call src-wildcard,*.png)
EPS_ILLUSTRATIONS=context-example.eps

IN_ITELY_FILES = $(call src-wildcard,snippets/*-intro.itely)
SNIPPET_LY_FILES = $(call src-wildcard,snippets/*.ly)

### Out files

# Dependencies
GENERATED_ITELY_FILES = $(IN_ITELY_FILES:snippets/%-intro.itely=$(outdir)/%.itely)

OUT_PDF_IMAGES = $(IMAGES:%.png=$(outdir)/%.pdf)
OUT_PDF_IMAGES += $(EPS_ILLUSTRATIONS:%.eps=$(outdir)/%.pdf)
OUT_PNG_IMAGES = $(OUT_PDF_IMAGES:%.pdf=%.png)

XREF_MAPS_FILES += $(XREF_MAPS_DIR)/internals.xref-map
OMF_FILES += $(outdir)/internals.html.omf $(outdir)/internals.pdf.omf

# Main manuals
TEXINFO_MANUALS = internals\
 $(TELY_FILES:%.tely=%)\
 $(TEXI_FILES:%.texi=%)

## CHAIN_RULE hack: keep the following line *before* including
# stepmake.make, so the Internals Reference is built before the
# Notation Reference, thus providing automatically generated sections
# of the NR
TEXI_FILES_FROM_TELY = $(outdir)/internals.texi

PDF_FILES = $(TEXINFO_MANUALS:%=$(outdir)/%.pdf)

UNSPLITTED_HTML_MANUALS = changes
SPLITTED_HTML_MANUALS = $(foreach manual, $(TEXINFO_MANUALS),\
 $(if $(findstring $(manual), $(UNSPLITTED_HTML_MANUALS)),,$(manual)))

OUT_HTML_FILES = $(UNSPLITTED_HTML_MANUALS:%=$(outdir)/%.html)\
 $(SPLITTED_HTML_MANUALS:%=$(outdir)/%-big-page.html)
DEEP_HTML_FILES = $(SPLITTED_HTML_MANUALS:%=$(outdir)/%/index.html)

# Symlinks to refer to external source documents from split and non-split HTML
source-links = $(outdir)/source\
 $(foreach manual, $(SPLITTED_HTML_MANUALS), $(outdir)/$(manual)/source)

# Other out files

HTML_PAGE_NAMES= index translations devel
OUT_HTML_FILES += $(HTML_PAGE_NAMES:%=$(outdir)/%.html)
OUT_CSS_FILES = $(CSS_FILES:%.css=$(outdir)/%.css)

MAIN_INFO_DOC = lilypond-notation
INFO_DOCS = lilypond-application lilypond-changes lilypond-contributor lilypond-internals \
 lilypond-essay lilypond-learning lilypond-notation music-glossary
ifeq ($(out),www)
INFO_DOCS += lilypond-snippets 
endif
INFO_FILES = $(INFO_DOCS:%=$(outdir)/%.info)

ifeq ($(out),www)
INFO_IMAGES_DIR = lilypond
DEST_INFO_IMAGES_SUBDIR = Documentation/
endif

include $(depth)/make/stepmake.make

OUT_TXT_FILES = $(addprefix $(outdir)/, $(addsuffix .txt, $(README_TOP_FILES)))

###########
### Targets

default: local-txt-doc

local-help: extra-local-help

extra-local-help:
	@echo -e "\
  check-xrefs [ISOLANG=LL] validate Texinfo cross-references\n\
  fix-xrefs [ISOLANG=LL]   interactively fix Texinfo cross-references (use with caution)\n\
  info        update info pages\n\
  xml	      update Docbook xml documentation\n\
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

info: $(INFO_FILES)
	@echo export LILYPOND_DATADIR=$(LILYPOND_DATADIR)
	@echo export PYTHONPATH=$(PYTHONPATH)

xml: $(outdir)/notation/notation.xml $(outdir)/internals/internals.xml


local-clean:
	rm -f $(INFO_IMAGES_DIR)

### Web targets

ifeq ($(out),www)
local-WWW-1: $(TEXINFO_MANUALS:%=$(outdir)/%.texi) $(PDF_FILES) info

local-WWW-2: txt-to-html $(OUT_HTML_FILES) $(DEEP_HTML_FILES) $(source-links) $(OMF_FILES)
endif


#########
### Rules

# cd $(outdir) rule gets bit hairy for --srcdir configure builds
txt-to-html:
	$(foreach a, $(README_TOP_FILES), cp $(top-src-dir)/$(a) $(outdir)/$(a).txt && ) true
	$(PYTHON) $(step-bindir)/text2html.py $(OUT_TXT_FILES)


$(outdir)/%.css: %.css
	ln -f $< $@

## Extra images dependencies
$(outdir)/learning/index.html: $(OUT_PNG_IMAGES) $(OUT_EPS_IMAGES)
$(outdir)/notation/index.html: $(OUT_PNG_IMAGES) $(OUT_EPS_IMAGES)
$(outdir)/learning-big-page.html: $(OUT_PNG_IMAGES)
$(outdir)/notation-big-page.html: $(OUT_PNG_IMAGES)

# Ugh, using '%' twice not possible
$(outdir)/notation/notation.xml: $(outdir)/notation.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$(dir $@) --docbook $<

$(outdir)/internals/internals.xml: $(outdir)/internals.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$(dir $@) --docbook $<

$(outdir)/learning.texi $(outdir)/notation.texi: $(OUT_PDF_IMAGES)

$(foreach manual, $(MANUAL_SUBDIRS),\
$(eval $(outdir)/(manual).texi: $(call src-wildcard,$(manual)/*.ite??)))

$(outdir)/%.png: %.png
	convert -depth 8 -geometry 50x50% $< $@

$(outdir)/%.png: %.eps
	gs -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -q -sOutputFile=$@ -sDEVICE=png16m -dEPSCrop -dNOPAUSE -f $< -c quit

$(outdir)/%.pdf: %.png
	convert -depth 8 $< $@

$(outdir)/%.pdf: %.eps
	gs -dAutoRotatePages=/None -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -sOutputFile=$@ -dEPSCrop -c .setpdfwrite -f $<


$(outdir)/source:
	@rm -f $(@)
	ln -sf $(depth) $(@)

$(outdir)/%/source:
	@rm -f $(@)
	mkdir -p $(dir $@)
	ln -sf $(depth)/.. $(@)


## Snippets rules idiosyncrases
$(outdir)/%.itely: snippets/%-intro.itely snippets/%.snippet-list
	xargs $(LYS_TO_TELY) -f doctitle,texidoc,verbatim --name=$@ --template=$< < $(filter %.snippet-list, $^)

$(outdir)/snippets.texi: $(GENERATED_ITELY_FILES) $(SNIPPET_LY_FILES)

$(outdir)/%.bib: %.bib
	ln -f $< $@

## notation.texi deps
$(top-build-dir)/mf/$(outconfbase)/feta16list.ly:
	$(MAKE) -C $(top-src-dir)/mf

$(outdir)/notation.texi: $(outdir)/ly-grammar.txt

## Rules for the automatically generated documentation
$(outdir)/ly-grammar.txt: $(top-src-dir)/lily/parser.yy
	cd $(outdir) && $(BISON) -v $<
	$(auxscript-dir)/yyout2grammar.py $(outdir)/parser.output $@

# There used to be a dependency on a dummy target, to force a rebuild
# of internals every time.  however, this triggers
# compilation during install, which is a bad thing (tm).

$(outdir)/internals.texi: $(LILYPOND_BINARY)
	cd $(outdir) && $(LILYPOND_BINARY) --verbose $(top-src-dir)/ly/generate-documentation


###############################################
#  Documentation and translations maintenance #
###############################################
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
	mkdir -p $(ISOLANG)/learning
	cp fr/GNUmakefile $(ISOLANG)
	cp fr/learning/GNUmakefile $(ISOLANG)/learning
	sed -i -e 's/ISOLANG *= *fr/ISOLANG = $(ISOLANG)/' $(ISOLANG)/GNUmakefile $(ISOLANG)/learning/GNUmakefile
	$(auxscript-dir)/texi-langutils.py -d $(outdir) -l $(ISOLANG) -o doc.pot --skeleton --gettext ../learning.tely
	mv $(outdir)/*.*tely $(ISOLANG)/learning
	msgmerge -U po/lilypond-doc.pot $(outdir)/doc.pot
	cp po/lilypond-doc.pot po/$(ISOLANG).po
	@echo "***  Please add a language definition for $(ISOLANG) in python/langdefs.py  ***"

CHECKED_FILES = $(ISOLANG)/index.html.in $(foreach i,learning notation application, \
  $(shell find $(ISOLANG)/$(manual) -name '*.*te??' -not -wholename '*out-www*')) \
  $(shell find $(ISOLANG)/texidocs/ -name '*.texidoc')

TELY_FILES = $(call src-wildcard,$(ISOLANG)/*.tely)
skeleton-update:
	$(auxscript-dir)/texi-langutils.py -d $(outdir) -l $(ISOLANG) --skeleton $(TELY_FILES:$(ISOLANG)/%.tely=../%.tely)
	$(auxscript-dir)/texi-skeleton-update.py $(ISOLANG) $(outdir)

snippet-update:
	$(auxscript-dir)/update-snippets.py user $(ISOLANG) '*.itely'

DOCUMENTS_INCLUDES:=-I $(ISOLANG) \
-I $(top-build-dir)/Documentation/$(ISOLANG)/out-www \
-I $(top-src-dir)/Documentation/snippets \
-I $(top-build-dir)/Documentation/out-www

else # ISOLANG is empty

DOCUMENTS_INCLUDES:=-I . \
-I $(top-build-dir)/Documentation/out-www \
-I $(top-src-dir)/Documentation/snippets \
-I $(top-build-dir)/Documentation/snippets/out-www

endif # ISOLANG

check-xrefs:
	$(auxscript-dir)/check_texi_refs.py --batch \
	$(DOCUMENTS_INCLUDES) $(auxpython-dir)/manuals_definitions.py

fix-xrefs:
	$(auxscript-dir)/check_texi_refs.py --auto-fix \
	$(DOCUMENTS_INCLUDES) $(auxpython-dir)/manuals_definitions.py

check-translation:
	ISOLANG=$(ISOLANG) $(auxscript-dir)/check_translation.py $(CHECK_TRANSLATION_FLAGS) $(CHECKED_FILES)

update-translation:
	ISOLANG=$(ISOLANG) $(auxscript-dir)/check_translation.py --update $(CHECK_TRANSLATION_FLAGS) $(CHECKED_FILES)

translation-status:
	make -C po out=www messages
	$(auxscript-dir)/translations-status.py

.SECONDARY:
