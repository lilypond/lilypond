depth = ..

########################
#  Documentation build #
########################

#############
### Variables

SUBDIRS = po
TEMPLATES = lilypond ly lilypond-book

### ly examples

LY_EXAMPLES = $(call src-wildcard,ly-examples/*.ly)
OUT_LY_EXAMPLES = $(LY_EXAMPLES:%.ly=$(outdir)/%.ly)
OUT_BITMAP_IMAGES += \
  $(LY_EXAMPLES:%.ly=$(outdir)/%.png) \
  $(LY_EXAMPLES:%.ly=$(outdir)/%-small.png)
LILYPOND_PREVIEW = $(LILYPOND_BINARY) $(LOCAL_LILYPOND_FLAGS) \
	-dpreview -dresolution=150

### Pictures

BITMAP_IMAGES = \
  $(call src-wildcard,pictures/*.png) \
  $(call src-wildcard,pictures/*.jpg)
EPS_ILLUSTRATIONS = $(call src-wildcard,pictures/*.eps)
SVG_FILES = $(call src-wildcard,pictures/*.svg)
PDF_DIR = $(call src-wildcard,pictures/pdf/*.pdf)

OUT_PDF_IMAGES = $(EPS_ILLUSTRATIONS:%.eps=$(outdir)/%.pdf)
OUT_BITMAP_IMAGES += $(BITMAP_IMAGES:%=$(outdir)/%)
OUT_BITMAP_IMAGES += $(EPS_ILLUSTRATIONS:%.eps=$(outdir)/%.png)

### Miscellaneous files

MISC_FILES = \
  ANNOUNCE-0.1 ANNOUNCE-1.0 ANNOUNCE-1.2 \
  announce-v2.0.html announce-v2.10.html announce-v2.12.ca.html \
    announce-v2.12.de.html announce-v2.12.es.html announce-v2.12.fr.html \
    announce-v2.12.html announce-v2.2.html announce-v2.4.html \
    announce-v2.6.html announce-v2.8.html \
  browser-language.ca.html browser-language.de.html \
    browser-language.es.html browser-language.fr.html \
    browser-language.html browser-language.it.html \
    browser-language.ja.html \
  ChangeLog-1.5 ChangeLog-2.1 ChangeLog-2.10 ChangeLog-2.3 \
  CHANGES-0.0 CHANGES-0.1 CHANGES-1.0 CHANGES-1.1 CHANGES-1.2 CHANGES-1.3 \
    CHANGES-1.4 CHANGES-1.5 \
  NEWS-1.2 NEWS-1.4 \
  THANKS-1.8 THANKS-2.0 THANKS-2.10 THANKS-2.12 THANKS-2.14 THANKS-2.16 \
    THANKS-2.18 THANKS-2.2 THANKS-2.4 THANKS-2.6 THANKS-2.8

OUT_MISC_FILES = $(addprefix $(outdir)/misc/,$(MISC_FILES))
OUT_CSS_FILES = \
  $(addprefix $(outdir)/,$(call src-wildcard,css/*.css) css/lilypond-highlighting.css)

### Snippets
#
# The base files (`.ly` with english doc) are synced in from the LSR project.
# Each language adds translated titles under `LANGUAGE/texidocs/`.  All
# translated titles are merged by `makesnippets.py`.  `lilypond-book` texi
# language support picks up the right header based on document language.

IN_ITELY_FILES = $(call src-wildcard,snippets/*-intro.itely)
SNIPPET_LY_FILES = $(call src-wildcard,snippets/*.ly)

### Out files

LANG_TELY_FILES = $(call src-wildcard,$(1)/*.tely)
LANG_TEXI_FILES = $(call src-wildcard,$(1)/*.texi)

DOC_TELY_FILES = $(foreach L, $(LANGS), $(call LANG_TELY_FILES,$(L)))
DOC_TEXI_FILES = $(foreach L, $(LANGS), $(call LANG_TEXI_FILES,$(L)))

DOC_BASENAMES = \
  contributor \
  changes \
  essay \
  extending \
  internals \
  learning \
  music-glossary \
  notation \
  snippets \
  usage \
  web

# Main manuals
TEXINFO_MANUALS = \
  en/internals \
  $(DOC_TELY_FILES:%.tely=%) \
  $(DOC_TEXI_FILES:%.texi=%)

# In info, images will be loaded from this dir.
INFO_IMAGES_DIR = lilypond

# Some PDF files require extra packages to build.
#
# Chinese/Japanese - needs:
#
#   XeTeX 0.9998+ (TeX Live 2012+) or luatex 0.95+
#     The modern TeX engine used for LilyPond's PDF documents.  It might be
#     contained in the 'texlive-xetex' or 'texlive-luatex' packages,
#     respectively.
#
#   IPAex fonts
#     Japanese fonts.  It might be contained in the 'texlive-lang-japanese'
#     package.
#
#   zhspacing
#     Spacing for mixed CJK-English documents using XeTeX.  It might be
#     contained in the 'texlive-lang-chinese' package.  (This package is for
#     Chinese, but can also be used for Japanese.)
#
# Note: Even if Japanese PDF documents have been built, links from HTML
#       documents to Japanese PDFs are not created.

ALL_PDF_FILES = $(TEXINFO_MANUALS:%=$(outdir)/%.pdf)

# Uncomment below to build PDF in cs/ja/zh.
# PDF_FILES = $(ALL_PDF_FILES)
PDF_FILES = \
  $(filter-out \
    $(outdir)/cs/% $(outdir)/zh/% $(outdir)/ja/%, \
    $(ALL_PDF_FILES))

SPLIT_HTML_INDEX_FILES = $(TEXINFO_MANUALS:%=$(outdir)/%/index.html)
BIG_HTML_FILES = $(TEXINFO_MANUALS:%=$(outdir)/%-big-page.html)

# This list must stay in sync with the corresponding @direntry block in file
# `web.texi`.
INFO_DOCS = \
  lilypond-changes \
  lilypond-contributor \
  lilypond-essay \
  lilypond-extending \
  lilypond-internals \
  lilypond-learning \
  lilypond-notation \
  lilypond-usage \
  music-glossary
# Some info files should only be processed if `$(out)` is equal to `www`, but we
# generate them anyway.  Reason is that we use repeated calls to `install-info`
# during installation to properly remove old entries in info's `dir` file, and
# older LilyPond versions inserted more entries that are no longer present.
ALL_INFO_DOCS = \
  $(INFO_DOCS) \
  lilypond-snippets \
  lilypond-web
ifeq ($(out),www)
  INFO_DOCS += lilypond-snippets \
               lilypond-web
endif
INFO_FILES = $(INFO_DOCS:%=$(outdir)/%.info)
ALL_INFO_FILES = $(ALL_INFO_DOCS:%=$(outdir)/%.info)

include $(depth)/make/lilypond.make

# Must be here, or overwritten by `lilypond-book-var`.
OUT_HTML_FILES = $(SPLIT_HTML_INDEX_FILES) $(BIG_HTML_FILES)

### Programs and flags

target_language = $(word 2, $(subst /, ,$@))

LILYPOND_BOOK_INCLUDE_DIRS = \
  $(outdir) \
  $(src-dir)/$(call target_language) \
  $(src-dir)/pictures \
  $(src-dir)/en/included \
  $(src-dir)

LILYPOND_BOOK_INCLUDES = $(addprefix -I ,$(LILYPOND_BOOK_INCLUDE_DIRS))

LILYPOND_BOOK_FLAGS += \
  --highlight

EXTRACT_TEXI_FILENAMES_FLAGS += $(if $(VERBOSE),,--quiet)

DOCUMENTATION_INCLUDE_DIRS = \
  $(src-dir)/pictures \
  $(dir $@) \
  $(outdir)/en \
  $(outdir) \
  $(src-dir)

DOCUMENTATION_INCLUDES = $(addprefix -I ,$(DOCUMENTATION_INCLUDE_DIRS))

# `check_texi_refs.py` wants `.tely` files first.
ifneq ($(ISOLANG),)
  XREF_INCLUDE_DIRS = $(ISOLANG) en .
else
  XREF_INCLUDE_DIRS = en .
endif

XREF_INCLUDES = $(addprefix -I ,$(XREF_INCLUDE_DIRS))

ifeq ($(WEB_VERSION),yes)
  TEXI2ANY_HTML_FLAGS += -D web_version
  TEXI2PDF_LOCAL_FLAGS += -D web_version
  # As version, use the released $(VERSION_DEVEL) instead of the in-development
  # $(MAJOR_VERSION).$(MINOR_VERSION).  This makes a difference after branching
  # for next stable release.
  WEB_ENV = ONLY_WEB_VERSION=v$(basename $(VERSION_DEVEL))
endif


###########
### Targets

default: $(ALL_INFO_FILES)

local-help: extra-local-help

extra-local-help:
	@echo "  check-xrefs [ISOLANG=LL] validate Texinfo cross-references"
	@echo "  fix-xrefs [ISOLANG=LL]   interactively fix Texinfo cross-references"
	@echo "                             (use with caution)"
	@echo "  info                     update info pages"
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


####
## Info targets (optionally with images)

info: $(ALL_INFO_FILES)

local-install: install-info

# process to install info:
#   remove old info file dir entries
#   remove old info files and image files
#   install new info files and optionally image files
#   add new info file dir entries

install-info: update-info-directory-file-install \
  install-info-images

uninstall-info: uninstall-info-files uninstall-info-images

update-info-directory-file-install: install-info-files
	cd $(DESTDIR)$(infodir) ; \
	  $(foreach f, $(INFO_DOCS), \
	    install-info --info-dir=$(DESTDIR)$(infodir) \
	      $(f).info > /dev/null 2>&1 ; ) true

install-info-files:  uninstall-info-files $(INFO_FILES)
	$(INSTALLPY) -d $(DESTDIR)$(infodir) ; \
	$(INSTALLPY) -c -m 644 \
	  $(addsuffix *,$(INFO_FILES)) $(DESTDIR)$(infodir)

uninstall-info-files: update-info-directory-file-uninstall
	rm -f $(addprefix $(DESTDIR)$(infodir)/, \
	        $(addsuffix *,$(INFO_DOCS)))

update-info-directory-file-uninstall:
	test ! -d $(DESTDIR)$(infodir) \
	|| ( cd $(DESTDIR)$(infodir) ; \
	    $(foreach f, $(ALL_INFO_DOCS), \
	      install-info --remove \
	        --info-dir=$(DESTDIR)$(infodir) \
	        $(f).info > /dev/null 2>&1 ; ) true )

ifeq ($(out),www)
  UPDATE_IMAGE_SYMLINKS = \
    mkdir -p $(outdir)/$(INFO_IMAGES_DIR) \
      && cd $(outdir)/$(INFO_IMAGES_DIR) \
      && find ../en -maxdepth 1 -name '[0-9a-f][0-9a-f]' \
              -exec sh -c 'ln -sf "$$@" .' sh {} +
else
  UPDATE_IMAGE_SYMLINKS = true
endif

install-info-images: $(INFO_FILES)
ifeq ($(out),www)
  # Remove $(infodir)/$(INFO_IMAGES_DIR) in case it is a symlink.
	  rm -rf $(DESTDIR)$(infodir)/$(INFO_IMAGES_DIR)
  ifneq ($(COPY_INFO_IMAGES),no)
	    $(INSTALLPY) -d $(DESTDIR)$(infodir)/$(INFO_IMAGES_DIR)
	    rsync \
	      --copy-links \
	      --recursive \
	      --include="*/" \
	      --include="*.png" \
	      --exclude="*" \
	      $(outdir)/lilypond/?? \
	      $(DESTDIR)$(infodir)/$(INFO_IMAGES_DIR)
  else # !COPY_INFO_IMAGES
	    $(INSTALLPY) -d $(DESTDIR)$(infodir)
	    cd $(DESTDIR)$(infodir) \
	      && ln -sf $$($(PYTHON) $(buildscript-dir)/relative.py \
	                     $(DESTDIR)$(webdir)/Documentation) \
	           $(INFO_IMAGES_DIR)
  endif # !COPY_INFO_IMAGES
else
	  true
endif # out=www

uninstall-info-images:
	rm -rf $(DESTDIR)$(infodir)/$(INFO_IMAGES_DIR)

### Web targets

PICTURE_SYMLINKS = $(foreach L, $(LANGS), $(outdir)/$(L)/pictures)

PDF_TARGETS = $(OUT_PDF_IMAGES) $(PDF_FILES)
pdf: $(PDF_TARGETS)

HTML_TARGETS = $(OUT_HTML_FILES)
HTML_MISC_TARGETS = \
  $(OUT_BITMAP_IMAGES) \
  $(OUT_CSS_FILES) \
  $(OUT_MISC_FILES) \
  $(OUT_LY_EXAMPLES)
html: $(HTML_TARGETS) $(HTML_MISC_TARGETS)

# Do not specify phony targets (pdf, html) as dependencies to avoid building the
# webdoc tree every time.
$(outdir)/webdoc/dep: \
  $(PDF_TARGETS) \
  $(PICTURE_SYMLINKS) \
  $(HTML_TARGETS) \
  $(HTML_MISC_TARGETS)
	$(call ly_progress,Making,$(dir $@),)
	cd $(outdir) \
	  && PERL=$(PERL) \
	     $(buildscript-dir)/build-doc-tree.sh \
	       webdoc "$(LANGS)" $(LYS_OUTPUT_DIR) "$(DOC_BASENAMES)"
	touch $@

local-WWW: info $(outdir)/webdoc/dep

WEB_HTML_TARGETS = \
  $(foreach L, \
            $(LANGS), \
            $(outdir)/$(L)/web/index.html $(outdir)/$(L)/web-big-page.html)
$(outdir)/website/dep: \
  $(WEB_HTML_TARGETS) \
  $(HTML_MISC_TARGETS) \
  $(PICTURE_SYMLINKS)
	$(call ly_progress,Making,$(dir $@),)
	cd $(outdir) \
	  && PERL=$(PERL) \
	     $(buildscript-dir)/build-doc-tree.sh website "$(LANGS)" "" "web"
	cp $(foreach F, \
	             favicon.ico robots.txt tweets.xml, \
	             $(src-dir)/webserver/$(F)) \
	  $(outdir)/website/web/
	cp $(src-dir)/webserver/website-dir.htaccess \
	  $(outdir)/website/web/.htaccess
	touch $@

website: $(outdir)/website/dep


#########
### Rules

$(outdir)/misc/%: misc/%
	$(call ly_progress,Copying,$@,< source)
	mkdir -p $(dir $@)
	cp -f $< $@

# Copy files while tracking their dependencies.
$(outdir)/ly-examples/%.ly: ly-examples/%.ly
	$(call ly_progress,Making,$@,(copy))
	mkdir -p $(dir $@)
	cp -f $< $@

$(outdir)/%.texi: %.texi
	$(call ly_progress,Making,$@,(copy))
	mkdir -p $(dir $@)
	cp -f $< $@

$(outdir)/%.txt: $(outdir)/%.texi $(outdir)/version.itexi $(outdir)/weblinks.itexi
	$(call ly_progress,Making,$@,< texi)
	$(buildscript-dir)/run-and-check.sh \
\
"$(TEXI2ANY) --plaintext \
   $(DOCUMENTATION_INCLUDES) \
   --no-split \
   --no-headers \
   --output $@ \
   $<" \
\
	  "$(outdir)/$*.texi2any-plaintext.log"

$(outdir)/version.itexi: $(top-src-dir)/VERSION
	$(call ly_progress,Making,$@,)
	$(PYTHON) $(buildscript-dir)/create-version-itexi.py > $@

$(outdir)/weblinks.itexi: $(top-src-dir)/VERSION
	$(call ly_progress,Making,$@,)
	$(PYTHON) $(buildscript-dir)/create-weblinks-itexi.py $(DOC_BASENAMES) > $@

$(outdir)/lilypond-%.info: \
  $(outdir)/en/%.texi \
  $(outdir)/version.itexi \
  $(outdir)/weblinks.itexi
	$(call ly_progress,Making,$@,< texi)
	$(UPDATE_IMAGE_SYMLINKS)
	$(buildscript-dir)/run-and-check.sh \
\
"$(TEXI2ANY) --info \
   $(DOCUMENTATION_INCLUDES) \
   --output=$@ \
   $<" \
\
	  "$(outdir)/$*.texi2any-info.log"

$(outdir)/music-glossary.info: \
  $(outdir)/en/music-glossary.texi \
  $(outdir)/version.itexi \
  $(outdir)/weblinks.itexi
	$(call ly_progress,Making,$@,< texi)
	$(UPDATE_IMAGE_SYMLINKS)
	$(buildscript-dir)/run-and-check.sh \
\
"$(TEXI2ANY) --info \
   $(DOCUMENTATION_INCLUDES) \
   --output=$@ \
   $<" \
\
	  "$(outdir)/music-glossary.texi2any-info.log"

$(outdir)/%.pdf: \
  $(outdir)/%.texi \
  $(outdir)/version.itexi \
  $(outdir)/weblinks.itexi \
  $(OUT_PDF_IMAGES) \
  $(OUT_BITMAP_IMAGES) \
  | pre-pdf
	$(call ly_progress,Making,$@,< texi)
	TEX="$(PDFTEX)" PDFTEX="$(PDFTEX)" PDFLATEX="$(PDFLATEX)" \
	  $(buildscript-dir)/run-and-check.sh \
\
"cd $(dir $@); \
   texi2pdf \
     $(TEXI2PDF_LOCAL_FLAGS) \
     -I ../ \
     -I $(abs-src-dir)/$(call target_language) \
     -I $(abs-src-dir) \
     -I $(abs-src-dir)/en/ \
     -o $(CURDIR)/$(outdir)/$*.tmp.pdf \
     $(CURDIR)/$< \
   < /dev/null" \
\
	    "$(outdir)/$*.texi2pdf.log"
# A surprisingly large number of options (the ones containing 'Image' in
# their names together with `ColorConversionStrategy`) is necessary to
# change Ghostscript's default behaviour of converting PNG images to JPEG,
# which would cause ugly artifacts in our screenshots.
#
# Option `Printed=false` makes Ghostscript preserve hyperlinks.
ifeq ($(USE_EXTRACTPDFMARK),yes)
	  $(EXTRACTPDFMARK) $(EXTRACTPDFMARK_OPTIONS) \
            -o $(outdir)/$*.pdfmark $(outdir)/$*.tmp.pdf
	  $(GS920) \
	    -dBATCH \
	    -dNOSAFER \
	    -dNOPAUSE \
	    -q \
	    -sDEVICE=pdfwrite \
	    -dAutoRotatePages=/None \
	    -dPrinted=false \
	    -dPreserveMarkedContent=true \
	    -dColorConversionStrategy=/LeaveColorUnchanged \
	    -dDownsampleMonoImages=false \
	    -dDownsampleGrayImages=false \
	    -dDownsampleColorImages=false \
	    -dAutoFilterColorImages=false \
	    -dAutoFilterGrayImages=false \
	    -dColorImageFilter=/FlateEncode \
	    -dGrayImageFilter=/FlateEncode \
	    -sOutputFile=$@ \
	    -c "30000000 setvmthreshold" \
	    -I $(top-build-dir)/out-fonts \
	    -I $(top-build-dir)/out-fonts/Font \
	    $(outdir)/$*.pdfmark \
	    $(outdir)/$*.tmp.pdf
	  rm $(outdir)/$*.tmp.pdf
else
	  mv $(outdir)/$*.tmp.pdf $@
endif

# LilyPond uses Cyrillic in its documentation; the used fonts are from the LH
# family.  By default, its metrics files are generated on the fly, which might
# cause problems in parallel make jobs.  For this reason we make TeX Live
# generate the needed TFM files in advance.
pre-pdf:
	$(MAKE) out=www -C tex local-WWW
	$(call ly_info,Making latt1095.tfm in TeX Live (mktextfm))
	$(buildscript-dir)/run-and-check.sh \
	  "mktextfm latt1095" "$(outdir)/latt1095.mktextfm.log"
	$(call ly_info,Making larm1095.tfm in TeX Live (mktextfm))
	$(buildscript-dir)/run-and-check.sh \
	  "mktextfm larm1095" "$(outdir)/larm1095.mktextfm.log"

# We use 'node' as the default split mode for HTML pages.  However, some
# versions of the NR use `@part`, making this split mode too low-level, so
# we replace it with 'section' mode for them.
TEXI2ANY_SPLIT := node
$(outdir)/en/notation/index.html: TEXI2ANY_SPLIT := section

$(outdir)/%/index.html: \
  $(outdir)/%.texi \
  $(outdir)/version.itexi \
  $(outdir)/weblinks.itexi \
  $(OUT_BITMAP_IMAGES) \
  $(OUT_CSS_FILES)
	$(call ly_progress,Making,$@,< texi)
	mkdir -p $(dir $@)
	rm -f $(dir $@)/pictures && ln -s ../pictures $(dir $@)/pictures
	rm -f $(dir $@)/css && ln -s ../../css $(dir $@)/css
	$(buildscript-dir)/run-and-check.sh \
\
"$(WEB_ENV) \
 DEPTH=$(depth)/../ \
   $(TEXI2ANY_HTML) \
     --split=$(TEXI2ANY_SPLIT) \
     $(DOCUMENTATION_INCLUDES) \
     $(TEXI2ANY_HTML_FLAGS) \
     --output=$(dir $@) \
     $<" \
\
	  "$(outdir)/$*.texi2any-html-split.log"

$(outdir)/%-big-page.html: \
  $(outdir)/%.texi \
  $(outdir)/version.itexi \
  $(outdir)/weblinks.itexi
	$(call ly_progress,Making,$@,< texi)
	$(buildscript-dir)/run-and-check.sh \
\
"DEPTH=$(depth) \
   $(TEXI2ANY_HTML) \
     $(DOCUMENTATION_INCLUDES) \
     --no-split \
     $(TEXI2ANY_HTML_FLAGS) \
     -D bigpage \
     --output=$@ \
     $<" \
\
	  "$(outdir)/$*.texi2any-html-big.log"

### Bad hack for badly-integrated roadmap.
# TODO: move into CG.
$(outdir)/en/ROADMAP: $(top-src-dir)/ROADMAP
	$(call ly_progress,Making,$@,(copy))
	mkdir -p $(dir $@)
	cp $(top-src-dir)/ROADMAP $@

$(outdir)/en/contributor.texi: $(outdir)/en/ROADMAP

$(outdir)/topdocs/NEWS.texi: $(outdir)/en/changes.texi
	mkdir -p $(dir $@)
	cp $< $@

### Bad hack for badly-integrated bibliography.
$(outdir)/%.itexi: bib/%.bib
	$(call ly_progress,Making,$@,< bib)
	BSTINPUTS=$(src-dir)/essay \
	  $(PYTHON) $(buildscript-dir)/bib2texi.py \
	    -s $(top-src-dir)/Documentation/lily-bib -o $@ $<

$(DOC_TELY_FILES:%.tely=$(outdir)/%.texi):

$(outdir)/learning.texi $(outdir)/notation.texi: $(OUT_PDF_IMAGES)

## Snippets rules idiosyncrasies.
# The snippets are generated by combining the `.ly` files in snippets (imported
# from the LSR) with the translations in `${LANG}/texidocs/`.
$(outdir)/snippets/%.itely: snippets/%-intro.itely snippets/%.snippet-list
	$(call ly_progress,Making,$@,)
	mkdir -p $(dir $@)
	xargs $(LYS_TO_TELY) \
		--prefix=snippets/ \
		--fragment-options="doctitle,texidoc,verbatim" \
		--output=$@ \
		--include=$(dir $<) \
		--template=$< \
		< $(filter %.snippet-list, $^)

$(outdir)/snippets/dep: $(SNIPPET_LY_FILES)
	$(call ly_progress,Making,$@,)
	mkdir -p $(dir $@)
	$(PYTHON) $(buildscript-dir)/makesnippets.py \
	  $(src-dir)/snippets $(dir $@) "$(LANGS)" $(src-dir)
	mkdir -p out
	touch $@

$(outdir)/en/internals.texi: $(top-src-dir)/ly/generate-documentation.ly
	$(call ly_progress,Making,$@,)
	mkdir -p $(dir $@)
	cd $(dir $@) \
	  && $(buildscript-dir)/run-and-check.sh \
\
"$(LILYPOND_BINARY) \
   --verbose \
   $<" \
\
	       generate-documentation.log


## Different manuals within a language link to each other.
define per_lang_per_doc_deps
  # `lilypond-book` automatically generates `.dep` files, but only for `.itely`
  # files.
  $(outdir)/$(1)/$(2).texi: \
    $(call src-wildcard,$(1)/$(2)/*.itexi) \
    $(outdir)/version.itexi
endef


## Dependencies on the automatically generated content.
define per_lang_dep
  include $(wildcard $(outdir)/$(1)/*.dep)

  $(outdir)/$(1)/snippets.texi: \
    $(IN_ITELY_FILES:snippets/%-intro.itely=$(outdir)/snippets/%.itely) \
    $(outdir)/snippets/dep

  $(outdir)/$(1)/notation.texi: $(outdir)/snippets/dep

  $(outdir)/$(1)/learning.texi: $(outdir)/snippets/dep

  $(outdir)/$(1)/usage.texi: $(outdir)/snippets/dep

  $(outdir)/$(1)/pictures:
	  rm -f $$@ && mkdir -p $$(dir $$@) && ln -s ../pictures $$@

  # Adjust the @direntry block to only list installed info files.
  $(outdir)/$(1)/web.texi: \
    $(1)/web.texi \
    $(outdir)/we-wrote.itexi \
    $(outdir)/others-did.itexi \
    $(outdir)/weblinks.itexi \
    $(outdir)/version.itexi
  ifneq ($(out),www)
	  $(call ly_progress,Making,$$@,(sed))
	  mkdir -p $$(dir $$@)
	  sed -e '/^\*.*lilypond-snippets/d' \
	      -e '/^\*.*lilypond-web/d' \
	      < $$< \
	      > $$@
  endif

  $(outdir)/$(1)/essay.texi: \
    $(outdir)/colorado.itexi \
    $(outdir)/computer-notation.itexi \
    $(outdir)/engravingbib.itexi

  # `notation.tely` includes generated files, including `hyphenation.itexi`,
  # as well as many like `markup-commands.tely` via
  # `notation/notation-appendices.itely`.  These are the result of processing
  # `ly/generate-documentation.ly` for `internals.texi` (see above).
  $(outdir)/$(1)/notation.texi: $(outdir)/en/internals.texi

  $(foreach D, \
            $(DOC_BASENAMES), \
            $(eval $(call per_lang_per_doc_deps,$(1),$(D))))
endef

$(foreach L, $(LANGS), $(eval $(call per_lang_dep,$(L))))


###############################################
#  Documentation and translations maintenance #
###############################################
po-update:
	$(MAKE) -C po po-update

ifneq ($(NO_COLOR),)
  CHECK_TRANSLATION_FLAGS = --no-color
endif

ifneq ($(ISOLANG),)
  # Also for updating/adding missing files.
  new-lang:
	  mkdir -p $(ISOLANG)/en
	  cp $(TRANSLATION_UNPROCESSED_FILES:%=en/%) $(ISOLANG)/en
	  $(PYTHON) $(auxscript-dir)/texi-langutils.py \
	    --output-dir $(ISOLANG) \
	    --language $(ISOLANG) \
	    --output-po doc.pot \
	    --skeleton \
	    --gettext \
	    -I DOCUMENTATION_INCLUDES \
	    $(TRANSLATION_DOCS)
	  mv $(ISOLANG)/en/* $(ISOLANG)
	  rmdir $(ISOLANG)/en
	  msgmerge -U po/lilypond-doc.pot $(ISOLANG)/doc.pot
	  test -e po/$(ISOLANG).po || cp po/lilypond-doc.pot po/$(ISOLANG).po
	  @echo "***  Please add language entries for $(ISOLANG) to the following files:"
	  @echo "***    python/langdefs.py"
	  @echo "***    Documentation/lilypond-lang.init"
	  @echo "***    Documentation/webserver/lilypond.org.htaccess"
	  @echo "***    scripts/build/create-weblinks-itexi.py"
	  @echo "***    ROADMAP"
	  @echo "***"
	  @echo "***  Please add a Texinfo language definition file"
	  @echo "***    (Documentation/tex/txi-$(ISOLANG).tex)"

  EN_TELY_FILES = $(call src-wildcard,en/*.tely)
  EN_TEXI_FILES = $(call src-wildcard,en/*.texi)

  TRANSLATION_DOCS = $(filter-out en/contributor.%, \
                       $(EN_TELY_FILES) $(EN_TEXI_FILES))
  TRANSLATED_FILES = $(shell git ls-files $(ISOLANG))

  TRANSLATION_UNPROCESSED_FILES = \
    cyrillic.itexi \
    common-macros.itexi \
    fdl.itexi \
    gpl.itexi \
    macros.itexi \
    search-box.ihtml

  snippet-update:
	  $(PYTHON) $(auxscript-dir)/update-snippets.py \
	    user $(ISOLANG) '*.itely'
endif # ISOLANG

check-xrefs:
	$(PYTHON) $(auxscript-dir)/check_texi_refs.py \
	  --batch \
	  --language=$(ISOLANG) \
	  $(XREF_INCLUDES)

fix-xrefs:
	$(PYTHON) $(auxscript-dir)/check_texi_refs.py \
	  --auto-fix \
	  --language=$(ISOLANG) \
	  $(XREF_INCLUDES)

check-translation:
	cd $(src-dir) \
	  && ISOLANG=$(ISOLANG) \
	     $(PYTHON) $(auxscript-dir)/check_translation.py \
	       $(CHECK_TRANSLATION_FLAGS) $(TRANSLATED_FILES)

update-translation:
	cd $(src-dir) \
	  && ISOLANG=$(ISOLANG) \
	     $(PYTHON) $(auxscript-dir)/check_translation.py \
	       --update $(CHECK_TRANSLATION_FLAGS) $(TRANSLATED_FILES)


#########
### Image rules

$(outdir)/%-flat-gray.png: %-flat-gray.png
	$(call ly_progress,Making,$@,(convert))
	mkdir -p $(dir $@)
	convert -strip -depth 8 -geometry 50x50% $< $@

$(outdir)/%-flat-bw.png: %-flat-bw.png
	$(call ly_progress,Making,$@,(convert))
	mkdir -p $(dir $@)
	convert -strip -depth 8 -geometry 50x50% $< $@

$(outdir)/%.png: %.png
	$(call ly_progress,Making,$@,)
	mkdir -p $(dir $@)
	cp $< $@

$(outdir)/%.jpg: %.jpg
	$(call ly_progress,Making,$@,)
	mkdir -p $(dir $@)
	cp $< $@

$(outdir)/%.png: %.eps
	$(call ly_progress,Making,$@,< eps)
	mkdir -p $(dir $@)
	gs -dAutoRotatePages=/None \
           -dPrinted=false \
           -dPreserveMarkedContent=true \
           -dTextAlphaBits=4 \
           -dGraphicsAlphaBits=4 \
           -q \
           -sOutputFile=$@ \
           -sDEVICE=png16m \
           -dEPSCrop \
           -dNOPAUSE \
           -f $< \
           -c quit

$(outdir)/%.pdf: %.eps
	$(call ly_progress,Making,$@,< eps)
	mkdir -p $(dir $@)
	gs -dAutoRotatePages=/None \
           -dPrinted=false \
           -dPreserveMarkedContent=true \
           -q \
           -sDEVICE=pdfwrite \
           -dNOPAUSE \
           -dBATCH \
           -sOutputFile=$@ \
           -dEPSCrop \
           -f $<

# ly-examples/
$(outdir)/%.png: %.ly
	$(call ly_progress,Making,$@,< ly)
	mkdir -p $(dir $@)
	$(buildscript-dir)/run-and-check.sh \
\
"$(LILYPOND_PREVIEW) \
   -o $(dir $@) \
   $<" \
\
	  "$(outdir)/$*.preview.log"
	mv $(outdir)/$*.preview.png $@

# Must have `ly-examples/`, to avoid conflicts with `xxx-small.png` from
# `pictures/`.
$(outdir)/ly-examples/%-small.png: $(outdir)/ly-examples/%.png
	$(call ly_progress,Making,$@,(convert))
	convert -strip -resize 600 -posterize 8 $< png8:$@

## LOGO

$(outdir)/%.ico: %.xpm
	$(call ly_progress,Making,$@,< xpm)
	mkdir -p $(dir $@)
	convert $< -define icon:auto-resize=64,48,32 $@

$(outdir)/%.css: %.css
	$(call ly_progress,Making,$@,(copy))
	mkdir -p $(dir $@)
	cp -f $< $@

$(outdir)/css/lilypond-highlighting.css:
	$(call ly_progress,Making,$@,(Pygments generation))
	mkdir -p $(dir $@)
	$(PYTHON) $(auxpython-dir)/book_highlight.py \
	  > $(outdir)/css/lilypond-highlighting.css
