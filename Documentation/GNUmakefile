depth = ..

########################
#  Documentation build #
########################

#############
### Variables

NAME = documentation
LANGS = $(shell $(PYTHON) $(top-src-dir)/python/langdefs.py)
MANUALS_SUBDIRS = application automated-engraving contributor essay general learning notation
SUBDIRS = $(MANUALS_SUBDIRS) snippets logo pictures misc po css topdocs $(LANGS)
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

IN_ITELY_FILES = $(call src-wildcard,snippets/*-intro.itely)
SNIPPET_LY_FILES = $(call src-wildcard,snippets/*.ly)

EXTRA_DIST_FILES = $(call src-wildcard,*.init) search-box.html

### Out files

# Dependencies
GENERATED_ITELY_FILES = $(IN_ITELY_FILES:snippets/%-intro.itely=$(outdir)/%.itely)

XREF_MAPS_FILES += $(XREF_MAPS_DIR)/internals.xref-map
OMF_FILES += $(outdir)/internals.html.omf $(outdir)/internals.pdf.omf

# Main manuals
TEXINFO_MANUALS = internals\
 $(TELY_FILES:%.tely=%)\
 $(TEXI_FILES:%.texi=%)
OUT_TEXINFO_MANUALS = $(TEXINFO_MANUALS:%=$(outdir)/%.texi)

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

MAIN_INFO_DOC = lilypond-notation
INFO_DOCS = lilypond-application lilypond-changes lilypond-contributor lilypond-internals \
 lilypond-essay lilypond-learning lilypond-notation music-glossary lilypond
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

### Web site idiosyncrases

$(outdir)/general/index.html: TEXI2HTML_INIT = $(WEB_TEXI2HTML_INIT)
$(outdir)/general/index.html: TEXI2HTML_SPLIT = $(WEB_TEXI2HTML_SPLIT)

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
local-WWW-1: $(OUT_TEXINFO_MANUALS) $(PDF_FILES) info

local-WWW-2: txt-to-html $(OUT_HTML_FILES) $(DEEP_HTML_FILES) $(source-links) $(OMF_FILES)
endif


#########
### Rules

$(outdir)/lilypond-%.info: $(outdir)/%.texi $(outdir)/$(INFO_IMAGES_DIR).info-images-dir-dep $(outdir)/version.itexi
	$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$@ $<

$(outdir)/lilypond.info: $(src-dir)/general.texi $(outdir)/$(INFO_IMAGES_DIR).info-images-dir-dep $(outdir)/version.itexi
	$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$@ $<

# cd $(outdir) rule gets bit hairy for --srcdir configure builds
txt-to-html:
	$(foreach a, $(README_TOP_FILES), cp $(top-src-dir)/$(a) $(outdir)/$(a).txt && ) true
	$(PYTHON) $(step-bindir)/text2html.py $(OUT_TXT_FILES)

ifeq ($(out),www)
## Extra images dependencies
$(OUT_TEXINFO_MANUALS): $(outdir)/pictures

$(outdir)/pictures:
	$(MAKE) -C pictures WWW-1
	ln -sf ../pictures/$(outdir) $@

$(outdir)/general.texi: $(outdir)/examples

$(outdir)/examples:
	$(MAKE) -C general/examples
	ln -sf ../general/examples/$(outdir) $@
endif

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
new-lang-dir:
	mkdir -p $(ISOLANG)/$(DIR)
	cp fr/GNUmakefile $(ISOLANG)
	cp fr/$(DIR)/GNUmakefile $(ISOLANG)/$(DIR)
	sed -i -e 's/ISOLANG *= *fr/ISOLANG = $(ISOLANG)/' $(ISOLANG)/GNUmakefile $(ISOLANG)/$(DIR)/GNUmakefile
	rm -f $(outdir)/*.*tely $(outdir)/*.*texi
	$(auxscript-dir)/texi-langutils.py -d $(outdir) -l $(ISOLANG) -o doc.pot --skeleton --gettext $$(cd $(ISOLANG) && ls -1 ../$(DIR).tely ../$(DIR).texi)
	for i in $$(cd $(outdir) && ls -1 $(DIR).tely $(DIR).texi); do\
	    test -e $(ISOLANG)/$$i || mv $(outdir)/$$i $(ISOLANG)/$$i;\
	done
	rm -f $(outdir)/$(DIR).tely $(outdir)/$(DIR).texi
	for i in $$(cd $(outdir) && ls -1 *.*tely *.*texi); do\
	    test -e $(ISOLANG)/$(DIR)/$$i\
		|| mv $(outdir)/$$i $(ISOLANG)/$(DIR)/$$i;\
	done

new-lang:
# Also for updating/adding missing files
	$(foreach i,$(TRANSLATION_DIRS),$(MAKE) new-lang-dir DIR=$(i) &&) :
	$(foreach i, $(EXTRA_TRANSLATION_FILES), \
	    (test -e $(ISOLANG)/$(i) \
		|| cp fr/$(i) $(ISOLANG)/$(i)) && ) true
	msgmerge -U po/lilypond-doc.pot $(outdir)/doc.pot
	test -e po/$(ISOLANG).po || cp po/lilypond-doc.pot po/$(ISOLANG).po
	@echo "***  Please add a language definition for $(ISOLANG) in python/langdefs.py  ***"

#TRANSLATION_DIRS = $(shell git ls-files $(ISOLANG) | $(PYTHON) $(buildscript-dir)/pytt.py '/[^/]*$$' / | sort -u )
TRANSLATION_DIRS = application general learning notation texidocs
TRANSLATION_FILES = $(shell git ls-files $(ISOLANG) | grep -v GNUmakefile)
EXTRA_TRANSLATION_FILES =\
 dedication.itely\
 index.html.in\
 common-macros.itexi\
 macros.itexi\
 translations.html.in\
 translations.template.html.in\
#


TELY_FILES = $(call src-wildcard,$(ISOLANG)/*.tely)
skeleton-update:
	mkdir -p $(ISOLANG)
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
	ISOLANG=$(ISOLANG) $(auxscript-dir)/check_translation.py $(CHECK_TRANSLATION_FLAGS) $(TRANSLATION_FILES)

update-translation:
	ISOLANG=$(ISOLANG) $(auxscript-dir)/check_translation.py --update $(CHECK_TRANSLATION_FLAGS) $(TRANSLATION_FILES)

translation-status:
	make -C po out=www messages
	$(auxscript-dir)/translations-status.py

.SECONDARY:
