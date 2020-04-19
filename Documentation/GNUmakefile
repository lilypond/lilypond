depth = ..

########################
#  Documentation build #
########################

#############
### Variables

NAME = documentation
LANGS = $(shell $(PYTHON) $(top-src-dir)/python/langdefs.py)
SUBDIRS = logo pictures misc po css topdocs \
  ly-examples $(LANGS)
STEPMAKE_TEMPLATES = documentation texinfo
LOCALSTEPMAKE_TEMPLATES = lilypond ly

### Extra flags

LILYPOND_BOOK_FLAGS = --extra-options '-e "(ly:set-option (quote internal-type-checking) \#t)"'
TEXI2PDF_FLAGS = \
  -I $(outdir) \
  -I $(top-build-dir)/Documentation/$(outconfbase)
$(outdir)/snippets-big-page.html: TEXI2HTML_FLAGS += -D short_toc
$(outdir)/snippets/index.html: TEXI2HTML_FLAGS += -D short_toc

### Extra source files

README_TOP_FILES= DEDICATION

IN_ITELY_FILES = $(call src-wildcard,snippets/*-intro.itely)
SNIPPET_LY_FILES = $(call src-wildcard,snippets/*.ly)
OUT_SNIPPET_LY_FILES = $(SNIPPET_LY_FILES:%.ly=out/%.ly)

### Out files

# Dependencies
GENERATED_ITELY_FILES = $(IN_ITELY_FILES:snippets/%-intro.itely=$(outdir)/%.itely)

XREF_MAPS_FILES += $(XREF_MAPS_DIR)/internals.xref-map

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

NOT_WEBSITE_TEXINFO_MANUALS = $(filter-out web,$(TEXINFO_MANUALS))
PDF_FILES = $(NOT_WEBSITE_TEXINFO_MANUALS:%=$(outdir)/%.pdf)

TOPDIR_HTML_MANUALS =
UNSPLIT_HTML_MANUALS =
SPLIT_HTML_MANUALS = $(foreach manual, $(TEXINFO_MANUALS),\
 $(if $(findstring $(manual), $(UNSPLIT_HTML_MANUALS)),,$(manual)))
NOT_TOPDIR_HTML_MANUALS = $(foreach manual, $(SPLIT_HTML_MANUALS),\
 $(if $(findstring $(manual), $(TOPDIR_HTML_MANUALS)),,$(manual)))

OUT_HTML_FILES = $(UNSPLIT_HTML_MANUALS:%=$(outdir)/%.html)\
 $(SPLIT_HTML_MANUALS:%=$(outdir)/%-big-page.html)
DEEP_HTML_FILES = $(NOT_TOPDIR_HTML_MANUALS:%=$(outdir)/%/index.html)

# Symlinks to refer to external source documents from split and non-split HTML
source-links = $(outdir)/source\
 $(foreach manual, $(SPLIT_HTML_MANUALS), $(outdir)/$(manual)/source)

# Other out files

MAIN_INFO_DOC = lilypond-web
INFO_DOCS = lilypond-usage lilypond-changes lilypond-contributor lilypond-internals \
 lilypond-essay lilypond-learning lilypond-notation music-glossary \
 lilypond-web lilypond-extending
ifeq ($(out),www)
INFO_DOCS += lilypond-snippets
endif
INFO_FILES = $(INFO_DOCS:%=$(outdir)/%.info)

ifeq ($(out),www)
INFO_IMAGES_DIR = lilypond
DEST_INFO_IMAGES_SUBDIR = Documentation
endif

include $(depth)/make/stepmake.make

DOCUMENTATION_INCLUDES += -I $(outdir) \
  -I $(top-build-dir)/Documentation/snippets/out \
  -I $(top-src-dir)/Documentation/included \
  -I $(top-src-dir)/Documentation/pictures \
  -I $(top-src-dir)/Documentation \
  -I $(top-src-dir)/input/regression

OUT_TXT_FILES = $(addprefix $(outdir)/, $(addsuffix .txt, $(README_TOP_FILES)))


### Web site idiosyncrasies
$(XREF_MAPS_DIR)/web.xref-map: XREF_MAP_FLAGS += --split=node

### AJAX scripts
JS_FILES = $(call src-wildcard,*.js)
PHP_FILES = $(call src-wildcard,*.php)

OUT_JS_FILES = $(JS_FILES:%.js=$(outdir)/%.js)
OUT_PHP_FILES = $(PHP_FILES:%.php=$(outdir)/%.php)



### bad hack for badly-integrated roadmap.
$(outdir)/ROADMAP:
	$(call ly_progress,Making,$@,(copy))
	cp $(top-src-dir)/ROADMAP $(outdir)
$(outdir)/contributor.texi: $(outdir)/ROADMAP

### bad hack for badly-integrated bibliography
$(outdir)/%.itexi: essay/%.bib
	$(call ly_progress,Making,$@,< bib)
	BSTINPUTS=$(src-dir)/essay $(PYTHON) $(buildscript-dir)/bib2texi.py \
		-s $(top-src-dir)/Documentation/lily-bib -o $@ -q $<

$(outdir)/essay.texi: \
  $(outdir)/colorado.itexi \
  $(outdir)/computer-notation.itexi \
  $(outdir)/engravingbib.itexi


###########
### Targets

default: local-txt-doc

local-help: extra-local-help

extra-local-help:
	@echo "  check-xrefs [ISOLANG=LL] validate Texinfo cross-references"
	@echo "  fix-xrefs [ISOLANG=LL]   interactively fix Texinfo cross-references"
	@echo "                             (use with caution)"
	@echo "  info                     update info pages"
	@echo "  xml                      update Docbook xml documentation"
	@echo
	@echo "Translations specific targets (see TRANSLATION for details):"
	@echo "  new-lang ISOLANG=LL           create and initialize"
	@echo "                                  subdirectory \'LL' for language \'LL"
	@echo "                                  (if \'LL exists, update missing file)"
	@echo "  po-update                     update and replace PO files for"
	@echo "                                  documentation with msgmerged versions"
	@echo "  check-translation ISOLANG=LL  show changes in English docs since"
	@echo "                                  last translation update"
	@echo "  update-translation ISOLANG=LL call $$$$EDITOR to help updating"
	@echo "                                  the translation"
	@echo "  snippet-update ISOLANG=LL     update ly snippets in Texinfo translation"
	@echo "                                  according to docs in English"
	@echo "                                  (use with caution)"
	@echo
	@echo "  LL refers to the desired locale"
	@echo "    (most often only the ISO 639 language code)."
	@echo

info: $(INFO_FILES)

xml: $(outdir)/notation/notation.xml $(outdir)/internals/internals.xml


local-clean:
	rm -f $(INFO_IMAGES_DIR)

### Web targets

ifeq ($(out),www)
local-WWW-1: $(OUT_TEXINFO_MANUALS) $(PDF_FILES) info

ifeq ($(AJAX_SEARCH),1)
local-WWW-2: txt-to-html $(OUT_HTML_FILES) $(DEEP_HTML_FILES) $(OUT_JS_FILES) $(OUT_PHP_FILES) $(source-links)
else
local-WWW-2: txt-to-html $(OUT_HTML_FILES) $(DEEP_HTML_FILES) $(source-links)
endif

endif


#########
### Rules

$(outdir)/lilypond-%.info: $(outdir)/%.texi $(outdir)/$(INFO_IMAGES_DIR).info-images-dir-dep $(outdir)/version.itexi $(outdir)/weblinks.itexi | $(OUT_TEXINFO_MANUALS)
	$(call ly_progress,Making,$@,< texi)
	$(buildscript-dir)/run-and-check.sh "$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$@ $<"  "$*.makeinfo.log"

txt-to-html: $(OUT_TXT_FILES) $(OUT_TXT_FILES:%.txt=%.html)

$(OUT_TXT_FILES): $(outdir)/%.txt: $(top-src-dir)/%
	$(call ly_progress,Making,$@,(copy))
	cp -f $< $@

$(OUT_TXT_FILES:%.txt=%.html): $(outdir)/%.html: $(outdir)/%.txt
	$(call ly_progress,Making,$@,< txt)
	$(PYTHON) $(buildscript-dir)/text2html.py $<

# Explicitly list the dependencies on generated content
$(outdir)/web.texi: $(outdir)/we-wrote.itexi $(outdir)/others-did.itexi $(outdir)/weblinks.itexi $(outdir)/version.itexi

ifeq ($(out),www)
## Extra images dependencies
$(OUT_TEXINFO_MANUALS): $(outdir)/pictures

$(outdir)/pictures:
	$(MAKE) -C pictures out=www WWW-1
	ln -sf ../pictures/$(outdir) $@

$(outdir)/web.texi: $(outdir)/css $(outdir)/ly-examples

$(outdir)/css:
	$(MAKE) -C css out=www WWW-1
	ln -sf ../css/$(outdir) $@

$(outdir)/ly-examples:
	$(MAKE) -C ly-examples out=www WWW-1
	ln -sf ../ly-examples/$(outdir) $@
endif


# Ugh, using '%' twice not possible
$(outdir)/notation/notation.xml: $(outdir)/notation.texi
	$(call ly_progress,Making,$@,< texi)
	mkdir -p $(dir $@)
	$(buildscript-dir)/run-and-check.sh "$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$(dir $@) --docbook $<"  "$*.makeinfoxml.log"

$(outdir)/internals/internals.xml: $(outdir)/internals.texi
	$(call ly_progress,Making,$@,< texi)
	mkdir -p $(dir $@)
	$(buildscript-dir)/run-and-check.sh "$(MAKEINFO) -I$(src-dir) -I$(outdir) --output=$(dir $@) --docbook $<"  "$*.makeinfoxml.log"

$(outdir)/learning.texi $(outdir)/notation.texi: $(OUT_PDF_IMAGES)

$(outdir)/source:
	@rm -f $(@)
	ln -sf $(depth) $(@)

$(outdir)/%/source:
	@rm -f $(@)
	mkdir -p $(dir $@)
	ln -sf $(depth)/.. $(@)


## Snippets rules idiosyncrasies
$(outdir)/%.itely: snippets/%-intro.itely snippets/%.snippet-list
	$(call ly_progress,Making,$@,)
	xargs $(LYS_TO_TELY) -f doctitle,texidoc,verbatim --name=$@ --template=$< < $(filter %.snippet-list, $^)

$(TEXI_FILES_FROM_TELY): $(GENERATED_ITELY_FILES) $(OUT_SNIPPET_LY_FILES)

$(OUT_SNIPPET_LY_FILES): out/ly-snippets.dep

out/ly-snippets.dep: $(SNIPPET_LY_FILES)
	$(call ly_progress,Making,$@,)
	mkdir -p snippets/out
	$(PYTHON) $(buildscript-dir)/makesnippets.py $(src-dir)/snippets snippets/out $(src-dir)
	mkdir -p out
	touch $@

$(outdir)/%.bib: %.bib
	$(call ly_progress,Making,$@,(hard link))
	ln -f $< $@

$(outdir)/%.js: %.js
	$(call ly_progress,Making,$@,(hard link))
	ln -f $< $@

$(outdir)/%.php: %.php
	$(call ly_progress,Making,$@,(hard link))
	ln -f $< $@

## Rules for the automatically generated documentation
$(outdir)/ly-grammar.txt: $(top-src-dir)/lily/parser.yy
	$(call ly_progress,Making,$@,< yy)
	cd $(outdir) && $(BISON) -v $<
	$(PYTHON) $(buildscript-dir)/yyout2grammar.py $(outdir)/parser.output $@

$(outdir)/internals.texi: $(LILYPOND_BINARY) $(INIT_LY_SOURCES) $(SCHEME_SOURCES)
	$(call ly_progress,Making,$@,)
	cd $(outdir) && $(buildscript-dir)/run-and-check.sh "$(LILYPOND_BINARY) --verbose $(top-src-dir)/ly/generate-documentation" "generate-documentation.log"

## Dependencies on the automatically generated documentation
$(outdir)/contributor.texi: $(outdir)/ly-grammar.txt

# notation.tely includes, via notation/notation-appendices.itely, many
# generated files (like markup-commands.tely). These are the result of
# processing ly/generate-documentation.ly for internals.texi (see above).
$(outdir)/notation.texi: $(outdir)/internals.texi

###############################################
#  Documentation and translations maintenance #
###############################################
po-update:
	$(MAKE) -C po po-update

ifneq ($(NO_COLOR),)
CHECK_TRANSLATION_FLAGS = --no-color
endif

ifneq ($(ISOLANG),)
new-lang-dir:
	mkdir -p $(ISOLANG)/$(DIR)
	rm -f $(outdir)/*.*tely $(outdir)/*.*texi
	$(PYTHON) $(auxscript-dir)/texi-langutils.py -d $(outdir) -l $(ISOLANG) -o doc.pot --skeleton --gettext $$(cd $(ISOLANG) && ls -1 ../$(DIR).tely ../$(DIR).texi)
	for i in $$(cd $(outdir) && ls -1 $(DIR).tely $(DIR).texi); do\
	    test -e $(ISOLANG)/$$i || mv $(outdir)/$$i $(ISOLANG)/$$i;\
	done
	rm -f $(outdir)/$(DIR).tely $(outdir)/$(DIR).texi
	for i in $$(cd $(outdir) && ls -1 *.*tely *.*texi); do\
	    test -e $(ISOLANG)/$(DIR)/$$i\
		|| test -e $(ISOLANG)/$$i\
		|| mv $(outdir)/$$i $(ISOLANG)/$(DIR)/$$i;\
	done

new-lang:
# Also for updating/adding missing files
	mkdir -p $(ISOLANG)/included
	if test -z "`ls $(ISOLANG)/included`"; then touch $(ISOLANG)/included/.gitignore; fi
	cp fr/GNUmakefile $(ISOLANG)
	sed -i -e 's/ISOLANG *= *fr/ISOLANG = $(ISOLANG)/' $(ISOLANG)/GNUmakefile
	$(foreach i,$(TRANSLATION_DIRS),$(MAKE) new-lang-dir DIR=$(i) &&) :
	msgmerge -U po/lilypond-doc.pot $(outdir)/doc.pot
	test -e po/$(ISOLANG).po || cp po/lilypond-doc.pot po/$(ISOLANG).po
	@echo "***  Please add a language definition for $(ISOLANG) in python/langdefs.py  ***"

TRANSLATION_DIRS = web texidocs
TRANSLATION_FILES = $(shell git ls-files $(ISOLANG) | grep -v GNUmakefile)

TELY_FILES = $(call src-wildcard,$(ISOLANG)/*.tely)

snippet-update:
	$(PYTHON) $(auxscript-dir)/update-snippets.py user $(ISOLANG) '*.itely'

DOCUMENTS_INCLUDES:=-I $(ISOLANG) \
-I $(top-build-dir)/Documentation/$(ISOLANG)/out-www \
-I $(top-src-dir)/Documentation/snippets \
-I $(top-build-dir)/Documentation/out-www \
-I $(top-build-dir)/Documentation/out

else # ISOLANG is empty

DOCUMENTS_INCLUDES:=-I . \
-I $(top-build-dir)/Documentation/out-www \
-I $(top-src-dir)/Documentation/snippets \
-I $(top-build-dir)/Documentation/snippets/out-www \
-I $(top-build-dir)/Documentation/out

endif # ISOLANG

check-xrefs:
	$(PYTHON) $(auxscript-dir)/check_texi_refs.py --batch \
	$(DOCUMENTS_INCLUDES) $(auxpython-dir)/manuals_definitions.py

fix-xrefs:
	$(PYTHON) $(auxscript-dir)/check_texi_refs.py --auto-fix \
	$(DOCUMENTS_INCLUDES) $(auxpython-dir)/manuals_definitions.py

check-translation:
	ISOLANG=$(ISOLANG) $(PYTHON) $(auxscript-dir)/check_translation.py $(CHECK_TRANSLATION_FLAGS) $(TRANSLATION_FILES)

update-translation:
	ISOLANG=$(ISOLANG) $(PYTHON) $(auxscript-dir)/check_translation.py --update $(CHECK_TRANSLATION_FLAGS) $(TRANSLATION_FILES)

# what's it gonna be, boy...tranlationS-/translation-
translation-status:
	PYTHONPATH=$(top-src-dir)/python:$(top-src-dir)/python/auxiliar $(PYTHON) $(top-src-dir)/scripts/auxiliar/translations-status.py
