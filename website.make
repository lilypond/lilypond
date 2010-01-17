################################################################
# website (without the rest of the docs)

################################################################
#####  SECURITY -- check these values for lilypond.org #########
################################################################
ifeq ($(WEBSITE_ONLY_BUILD),1)
  ### for lilypond.org
  top-src-dir=$(HOME)/src/lilypond
  depth=.
  trusted-dir=$(HOME)/lilypond/trusted-scripts
  script-dir=$(trusted-dir)
  texi2html-init-file=$(trusted-dir)/lilypond-texi2html.init
  TEXI2HTML_PROGRAM=$(HOME)/usr/bin/texi2html
  EXAMPLES=$(HOME)/media/ly-examples/
  PICTURES=$(HOME)/media/pictures
else
  ### for normal git
  script-dir=$(top-src-dir)/scripts/build/
  texi2html-init-file=$(top-src-dir)/Documentation/lilypond-texi2html.init
  include $(config_make)
  # I assume this is run from top-build-dir
  EXAMPLES=Documentation/web/ly-examples/out-www/
  PICTURES=Documentation/pictures/out-www/
endif


################################################################
OUT=out-website

TEXI2HTML=TOP_SRC_DIR=$(top-src-dir) DEPTH=$(depth) PERL_UNICODE=SD $(TEXI2HTML_PROGRAM)

EXTRACT_TEXI_FILENAMES=python $(script-dir)/extract_texi_filenames.py
CREATE_VERSION=python $(script-dir)/create-version-itexi.py
CREATE_WEBLINKS=python $(script-dir)/create-weblinks-itexi.py


# don't include web
MANUALS=$(wildcard $(top-src-dir)/Documentation/*.tely)
MANUALS+=$(top-src-dir)/Documentation/contributor.texi

website-test:
	echo $(TEXI2HTML)

website-version:
	mkdir -p $(OUT)
	$(CREATE_VERSION) $(top-src-dir) > $(OUT)/version.itexi
	$(CREATE_WEBLINKS) $(top-src-dir) > $(OUT)/weblinks.itexi

website-xrefs: website-version
	$(EXTRACT_TEXI_FILENAMES) -I $(top-src-dir)/Documentation/ \
		-I $(OUT) -o $(OUT) --split=node \
		$(top-src-dir)/Documentation/web.texi
	$(foreach manual, $(MANUALS), \
		$(EXTRACT_TEXI_FILENAMES) -I $(top-src-dir)/Documentation/ \
		-I $(OUT) -o $(OUT) $(manual) && ) :

website-texinfo: website-version website-xrefs
	$(TEXI2HTML) --prefix=index \
		--split=section \
		--I=$(top-src-dir)/Documentation/ \
		--I=$(OUT) \
		--init-file=$(texi2html-init-file) \
		-D web_version \
		--output=$(OUT)/website/ \
		$(top-src-dir)/Documentation/web.texi

website-css:
	cp $(top-src-dir)/Documentation/css/*.css $(OUT)/website/

website-pictures:
	mkdir -p $(OUT)/website/pictures/
	cp $(PICTURES)/* $(OUT)/website/pictures/
	ln -sf website/pictures $(OUT)/pictures

website-examples:
	mkdir -p $(OUT)/website/ly-examples
	cp $(EXAMPLES)/* $(OUT)/website/ly-examples


website: website-texinfo website-css website-pictures website-examples


