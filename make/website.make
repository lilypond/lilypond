################################################################
# website (without the rest of the docs)

################################################################
#####  SECURITY -- check these values for lilypond.org #########
################################################################

ifeq ($(WEBSITE_ONLY_BUILD),1)
  ### for lilypond.org
  TOP_SRC_DIR=$(LILYPOND_GIT)
  TRUSTED_DIR=$(HOME)/lilypond/trusted-scripts
  top-src-dir=$(TOP_SRC_DIR)
  depth=.
  trusted-dir=$(TRUSTED_DIR)
  script-dir=$(trusted-dir)
  texi2html-init-file=$(trusted-dir)/lilypond-texi2html.init
  top-htaccess=$(trusted-dir)/lilypond.org.htaccess
  dir-htaccess=$(trusted-dir)/website-dir.htaccess
  # grab it from PATH
  TEXI2HTML_PROGRAM=texi2html
  PYTHON=python
  PYTHONPATH=$(TRUSTED_DIR)
else
  ### for normal git
  script-dir=$(top-src-dir)/scripts/build
  texi2html-init-file=$(top-src-dir)/Documentation/lilypond-texi2html.init
  top-htaccess=$(top-src-dir)/Documentation/web/server/lilypond.org.htaccess
  dir-htaccess=$(top-src-dir)/Documentation/web/server/website-dir.htaccess
  include $(config_make)
endif

include $(top-src-dir)/VERSION

################################################################
#The 4 lines below present an option to force make website to run
# quietly only when it is run as make -s website.  However, we've
# decided not to use this switch, and run the scripts quietly all
# the time
################################################################
#quiet-run = $(findstring s, $(MAKEFLAGS))
#ifeq ($(quiet-run),s)
#  quiet-flag=-q
#endif

#Nothing clever here - just allows the use of a boolean to control
#  quiet running
quiet-run = true
ifeq ($(quiet-run),true)
  quiet-flag=-q
endif

################################################################
OUT=out-website

WEB_LANGS := $(shell MAKEWEB=1 $(PYTHON) $(top-src-dir)/python/langdefs.py)

TEXI2HTML=ONLY_WEB_VERSION=v$(MAJOR_VERSION).$(MINOR_VERSION) TOP_SRC_DIR=$(top-src-dir) DEPTH=$(depth) PERL_UNICODE=SD \
	$(TEXI2HTML_PROGRAM) -D web_version --prefix=index --split=section \
		--init-file=$(texi2html-init-file) \
		--I=$(dir $<) \
		--I=$(top-src-dir)/Documentation \
		--I=$(OUT) \
		--output=$(dir $@)

EXTRACT_TEXI_FILENAMES=$(PYTHON) $(script-dir)/extract_texi_filenames.py $(quiet-flag) \
	--known-missing-files=$(top-src-dir)/scripts/build/website-known-missing-files.txt \
		-I $(top-src-dir)/Documentation \
		-I $(dir $<) \
		-I $(OUT) \
		-o $(OUT)
CREATE_VERSION=$(PYTHON) $(script-dir)/create-version-itexi.py
CREATE_WEBLINKS=$(PYTHON) $(script-dir)/create-weblinks-itexi.py
MASS_LINK=$(PYTHON) $(script-dir)/mass-link.py
WEB_POST=$(PYTHON) $(script-dir)/website_post.py
WEB_BIBS=$(PYTHON) $(script-dir)/bib2texi.py

EXAMPLES=$(LILYPOND_WEB_MEDIA_GIT)/ly-examples
PICTURES=$(LILYPOND_WEB_MEDIA_GIT)/pictures
PDFS=$(LILYPOND_WEB_MEDIA_GIT)/pdf

SERVER_FILES=$(top-src-dir)/Documentation/web/server

# don't include web
MANUALS = $(MANUALS_TELY) $(MANUALS_TEXI) $(MANUALS_TRANSLATION)
MANUALS_TELY := $(notdir $(wildcard $(top-src-dir)/Documentation/*.tely))
MANUALS_TEXI := contributor.texi

# Harvest the translated manuals.
#   - Store each manual in a language-specific macro, e.g. when we find
#     de/learning.tely we add learning.tely to MANUALS_de,
#   - Store each manual with an added language suffix in MANUALS_TRANSLATION,
#     e.g. learning.de.tely for the German learning manual.
$(eval $(foreach l,$(WEB_LANGS),\
	$(eval MANUALS_$(l) := $(notdir $(wildcard $(top-src-dir)/Documentation/$(l)/*.tely))) \
	$(eval MANUALS_TRANSLATION += $(MANUALS_$(l):%.tely=%.$(l).tely)) \
))

# The web.texi manuals, English and translated
MANUALS_WEB := web.texi $(WEB_LANGS:%=web.%.texi)

# The basename of all manuals (basename includes the language suffix)
MANUALS_BASE = $(basename $(MANUALS) $(MANUALS_WEB))


#######################
### Dependency tracking

# Find the file $(1) within the texinfo include dirs and return its path.
# If not found, i.e. it is a generated file, then the file is ignored.
find-texi = \
$(firstword \
	$(wildcard $(dir $<)$(1)) \
	$(wildcard $(top-src-dir)/Documentation/$(1)) \
)

# Recursively scan the file $(1) for @include, search for included files
# within the texinfo include dirs, and return all dependencies.
scan-texi = \
	$(foreach f, $(shell echo | sed -ne "/^@include[[:space:]]/s/@include//p" $(1)), \
	$(call find-texi,$(f)) \
	$(call scan-texi,$(call find-texi,$(f))) \
)

# Find dependencies for the target $@, based on the texinfo source file $<,
# and write the dependencies to a .dep file.
DO_TEXI_DEP = ( mkdir -p $(dir $@) && echo ./$@: $(call scan-texi,$<) > $@.dep ) &&

# This is where we import the .dep files so that `make' knows about
# the various dependencies.
-include dummy.dep $(wildcard $(OUT)/*.dep) $(wildcard $(OUT)/*/*.dep)


###################
### Generated files

bib-files = $(OUT)/others-did.itexi $(OUT)/we-wrote.itexi

css-src-files := $(notdir $(wildcard $(top-src-dir)/Documentation/css/*.css))
css-files = $(css-src-files:%=$(OUT)/website/css/%)

example-src-files := $(notdir $(wildcard $(EXAMPLES)/*))
example-files = $(example-src-files:%=$(OUT)/website/ly-examples/%)

misc-src-files := $(notdir $(wildcard $(top-src-dir)/Documentation/misc/*.*))
misc-files += $(misc-src-files:%=$(OUT)/website/misc/%)

picture-src-files := $(notdir $(wildcard $(PICTURES)/*))
picture-files = $(picture-src-files:%=$(OUT)/website/pictures/%)

pdf-src-files := $(notdir $(wildcard $(PDFS)/*))
pdf-files = $(pdf-src-files:%=$(OUT)/website/pdf/%)

post-files = $(OUT)/website/index.html

root-files = $(OUT)/.htaccess \
             $(OUT)/website/.htaccess \
             $(OUT)/website/favicon.ico \
             $(OUT)/website/tweets.xml \
             $(OUT)/website/robots.txt

texinfo-files = $(OUT)/index.html $(WEB_LANGS:%=$(OUT)/%/index.html)

version-files = $(OUT)/version.itexi $(OUT)/weblinks.itexi

xref-files = $(MANUALS_BASE:%=$(OUT)/%.xref-map)


###########
### Targets

.PHONY: website website-bibs website-css website-examples website-misc \
        website-pictures website-post website-test website-texinfo \
        website-version website-xrefs check-setup website-pdf

check-setup:
ifeq ($(LILYPOND_WEB_MEDIA_GIT),)
	echo "Need a LILYPOND_WEB_MEDIA_GIT environment variable!"
	exit 1
endif

website: check-setup website-post website-examples website-pictures website-css website-misc website-pdf

website-bibs: website-version $(OUT) $(bib-files)

website-css: $(OUT)/website/css $(css-files)

website-examples: $(OUT)/website/ly-examples $(example-files)

website-misc: $(OUT)/website $(OUT)/website/misc $(misc-files) $(root-files)

website-pictures: $(OUT)/website/pictures $(OUT)/pictures $(picture-files)

website-pdf: $(OUT)/website/pdf $(pdf-files)

website-post: website-texinfo $(post-files)

website-test:
	echo $(TEXI2HTML)

website-texinfo: website-version website-xrefs website-bibs $(texinfo-files)

website-version: $(OUT) $(version-files)

website-xrefs: website-version $(OUT) $(xref-files)


#########
### Rules

# Directories
$(OUT) $(OUT)/website $(OUT)/website/css $(OUT)/website/ly-examples $(OUT)/website/misc $(OUT)/website/pdf $(OUT)/website/pictures: %:
	mkdir -p $@

$(OUT)/pictures: $(OUT)/website/pictures
	ln -sf website/pictures $(OUT)/pictures

# Generated itexi files
$(OUT)/version.itexi: $(top-src-dir)/VERSION
	$(CREATE_VERSION) $(top-src-dir) > $(OUT)/version.itexi

$(OUT)/weblinks.itexi: $(top-src-dir)/VERSION
	$(CREATE_WEBLINKS) $(top-src-dir) > $(OUT)/weblinks.itexi

$(bib-files): $(OUT)/%.itexi: $(top-src-dir)/Documentation/web/%.bib
	BSTINPUTS=$(top-src-dir)/Documentation/web \
		$(WEB_BIBS) -s web \
		-s $(top-src-dir)/Documentation/lily-bib \
		-o $@ \
		$(quiet-flag) \
		$<

# Get xrefs for English tely manuals
$(MANUALS_TELY:%.tely=$(OUT)/%.xref-map): $(OUT)/%.xref-map: $(top-src-dir)/Documentation/%.tely
	$(DO_TEXI_DEP) $(EXTRACT_TEXI_FILENAMES) $<

# Get xrefs for English texi manuals
$(MANUALS_TEXI:%.texi=$(OUT)/%.xref-map): $(OUT)/%.xref-map: $(top-src-dir)/Documentation/%.texi
	$(DO_TEXI_DEP) $(EXTRACT_TEXI_FILENAMES) $<

# Get xrefs for translated tely manuals
$(eval $(foreach l,$(WEB_LANGS),\
$(eval $(MANUALS_$(l):%.tely=$(OUT)/%.$(l).xref-map): $(OUT)/%.$(l).xref-map: $(top-src-dir)/Documentation/$(l)/%.tely; \
	$$(DO_TEXI_DEP) $$(EXTRACT_TEXI_FILENAMES) $$< ) \
))

# Get xrefs for the English web.texi manual
$(OUT)/web.xref-map: $(top-src-dir)/Documentation/web.texi
	$(DO_TEXI_DEP) $(EXTRACT_TEXI_FILENAMES) --split=node $<

# Get xrefs for translated web.texi manuals
$(OUT)/web.%.xref-map: $(top-src-dir)/Documentation/%/web.texi
	$(DO_TEXI_DEP) $(EXTRACT_TEXI_FILENAMES) --split=node $<

# Build the English website
$(OUT)/index.html: $(top-src-dir)/Documentation/web.texi $(version-files) $(xref-files)
	$(DO_TEXI_DEP) $(TEXI2HTML) $<

# Build translated websites
$(eval $(foreach l,$(WEB_LANGS),\
$(eval $(OUT)/$(l)/index.html: $(top-src-dir)/Documentation/$(l)/web.texi $(version-files) $(xref-files); \
	$$(DO_TEXI_DEP) $$(TEXI2HTML) --lang="$(l)" $$<; ) \
))

# Website post-processing
$(OUT)/website/index.html: $(wildcard $(OUT)/*.html)
	ls $(OUT)/*.html | sed 's!$(OUT)/!!g' | xargs $(MASS_LINK) --prepend-suffix="" hard $(OUT)/ $(OUT)/website/
	$(foreach l,$(WEB_LANGS), \
		ls $(OUT)/$(l)/*.html | xargs grep -L 'UNTRANSLATED NODE: IGNORE ME' | sed 's!$(OUT)/$(l)/!!g' | xargs $(MASS_LINK) --prepend-suffix=".$(l)" hard $(OUT)/$(l)/ $(OUT)/website/; )
	$(WEB_POST) $(OUT)/website

# Simple copy
$(css-files): $(OUT)/website/css/%: $(top-src-dir)/Documentation/css/%
	cp $< $@

$(example-files): $(OUT)/website/ly-examples/%: $(EXAMPLES)/%
	cp $< $@

$(misc-files): $(OUT)/website/misc/%: $(top-src-dir)/Documentation/misc/%
	cp $< $@

$(picture-files): $(OUT)/website/pictures/%: $(PICTURES)/%
	cp $< $@

$(pdf-files): $(OUT)/website/pdf/%: $(PDFS)/%
	cp $< $@

$(OUT)/website/favicon.ico: $(SERVER_FILES)/favicon.ico
	cp $< $@

$(OUT)/website/robots.txt: $(SERVER_FILES)/robots.txt
	cp $< $@

$(OUT)/website/tweets.xml: $(SERVER_FILES)/tweets.xml
	cp $< $@

$(OUT)/.htaccess: $(top-htaccess)
	cp $< $@

$(OUT)/website/.htaccess: $(dir-htaccess)
	cp $< $@
