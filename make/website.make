################################################################
# website (without the rest of the docs)

################################################################
#####  SECURITY -- check these values for lilypond.org #########
################################################################

ifeq ($(WEBSITE_ONLY_BUILD),1)
  ### for lilypond.org
  TOP_SRC_DIR=$(HOME)/lilypond/lilypond-git
  TRUSTED_DIR=$(HOME)/lilypond/trusted-scripts
  top-src-dir=$(TOP_SRC_DIR)
  depth=.
  trusted-dir=$(TRUSTED_DIR)
  script-dir=$(trusted-dir)
  texi2html-init-file=$(trusted-dir)/lilypond-texi2html.init
  top-htaccess=$(trusted-dir)/lilypond.org.htaccess
  dir-htaccess=$(trusted-dir)/website-dir.htaccess
  TEXI2HTML_PROGRAM=$(HOME)/usr/bin/texi2html
  EXAMPLES=$(HOME)/lilypond/media/ly-examples
  PICTURES=$(HOME)/lilypond/media/pictures
  PYTHON=python
  PYTHONPATH=$(TRUSTED_DIR)
else
  ### for normal git
  script-dir=$(top-src-dir)/scripts/build
  texi2html-init-file=$(top-src-dir)/Documentation/lilypond-texi2html.init
  top-htaccess=$(top-src-dir)/Documentation/web/server/lilypond.org.htaccess
  dir-htaccess=$(top-src-dir)/Documentation/web/server/website-dir.htaccess
  include $(config_make)
  # I assume this is run from top-build-dir
  EXAMPLES=Documentation/web/ly-examples/out-www
  PICTURES=Documentation/pictures/out-www
endif

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

TEXI2HTML=ONLY_WEB=1 TOP_SRC_DIR=$(top-src-dir) DEPTH=$(depth) PERL_UNICODE=SD \
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
CREATE_VERSION=python $(script-dir)/create-version-itexi.py
CREATE_WEBLINKS=python $(script-dir)/create-weblinks-itexi.py
MASS_LINK=python $(script-dir)/mass-link.py
WEB_POST=python $(script-dir)/website_post.py
WEB_BIBS=python $(script-dir)/bib2texi.py

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


###################
### Generated files

bib-files = $(OUT)/others-did.itexi $(OUT)/we-wrote.itexi

css-src-files := $(notdir $(wildcard $(top-src-dir)/Documentation/css/*.css))
css-files = $(css-src-files:%=$(OUT)/website/css/%)

example-src-files := $(notdir $(wildcard $(EXAMPLES)/*))
example-files = $(example-src-files:%=$(OUT)/website/ly-examples/%)

misc-files = $(OUT)/.htaccess \
             $(OUT)/website/.htaccess \
             $(OUT)/website/favicon.ico \
             $(OUT)/website/robots.txt

picture-src-files := $(notdir $(wildcard $(PICTURES)/*))
picture-files = $(picture-src-files:%=$(OUT)/website/pictures/%)

post-files = $(OUT)/website/index.html

texinfo-files = $(OUT)/index.html $(WEB_LANGS:%=$(OUT)/%/index.html)

version-files = $(OUT)/version.itexi $(OUT)/weblinks.itexi

xref-files = $(MANUALS_BASE:%=$(OUT)/%.xref-map)


###########
### Targets

.PHONY: website website-bibs website-css website-examples website-misc \
        website-pictures website-post website-test website-texinfo \
        website-version website-xrefs

website: website-post website-examples website-pictures website-css website-misc

website-bibs: website-version $(OUT) $(bib-files)

website-css: $(OUT)/website/css $(css-files)

website-examples: $(OUT)/website/ly-examples $(example-files)

website-misc: $(OUT)/website $(misc-files)

website-pictures: $(OUT)/website/pictures $(OUT)/pictures $(picture-files)

website-post: website-texinfo $(post-files)

website-test:
	echo $(TEXI2HTML)

website-texinfo: website-version website-xrefs website-bibs $(texinfo-files)

website-version: $(OUT) $(version-files)

website-xrefs: website-version $(OUT) $(xref-files)


#########
### Rules

# Directories
$(OUT) $(OUT)/website $(OUT)/website/css $(OUT)/website/ly-examples $(OUT)/website/pictures: %:
	mkdir -p $@

$(OUT)/pictures: $(OUT)/website/pictures
	ln -sf website/pictures $(OUT)/pictures

# Generated itexi files
$(OUT)/version.itexi: #FIXME: add dependencies
	$(CREATE_VERSION) $(top-src-dir) > $(OUT)/version.itexi

$(OUT)/weblinks.itexi: #FIXME: add dependencies
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
$(OUT)/index.html: $(top-src-dir)/Documentation/web.texi
	$(DO_TEXI_DEP) $(TEXI2HTML) $<

# Build translated websites
$(eval $(foreach l,$(WEB_LANGS),\
$(eval $(OUT)/$(l)/index.html: $(top-src-dir)/Documentation/$(l)/web.texi; \
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

$(picture-files): $(OUT)/website/pictures/%: $(PICTURES)/%
	cp $< $@

$(OUT)/website/favicon.ico: $(SERVER_FILES)/favicon.ico
	cp $< $@

$(OUT)/website/robots.txt: $(SERVER_FILES)/robots.txt
	cp $< $@

$(OUT)/.htaccess: $(top-htaccess)
	cp $< $@

$(OUT)/website/.htaccess: $(dir-htaccess)
	cp $< $@
