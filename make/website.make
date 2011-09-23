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

EXAMPLES=$(LILYPOND_WEB_MEDIA_GIT)/ly-examples
PICTURES=$(LILYPOND_WEB_MEDIA_GIT)/pictures

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

WEB_LANGS = $(shell MAKEWEB=1 $(PYTHON) $(top-src-dir)/python/langdefs.py)

TEXI2HTML=ONLY_WEB=1 TOP_SRC_DIR=$(top-src-dir) DEPTH=$(depth) PERL_UNICODE=SD $(TEXI2HTML_PROGRAM)

EXTRACT_TEXI_FILENAMES=python $(script-dir)/extract_texi_filenames.py
CREATE_VERSION=python $(script-dir)/create-version-itexi.py
CREATE_WEBLINKS=python $(script-dir)/create-weblinks-itexi.py
MASS_LINK=python $(script-dir)/mass-link.py
WEB_POST=python $(script-dir)/website_post.py
WEB_BIBS=python $(script-dir)/bib2texi.py

SERVER_FILES=$(top-src-dir)/Documentation/web/server

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
	for l in '' $(WEB_LANGS); do \
		len="$${#l}" ; \
		r="$$l"; \
		if [ "$$len" -gt "0" ] ; then \
			r="$$r"/; \
		fi ; \
		$(EXTRACT_TEXI_FILENAMES) \
			-I $(top-src-dir)/Documentation \
			-I $(top-src-dir)/Documentation/"$$l" \
			-I $(OUT) -o $(OUT) --split=node \
			--known-missing-files=$(top-src-dir)/scripts/build/website-known-missing-files.txt \
			$(quiet-flag) \
			$(top-src-dir)/Documentation/"$$l"/web.texi ;\
		for m in $(MANUALS); do \
			n=`echo "$$m" | sed 's/Documentation/Documentation\/'$$l'/'` ; \
			b=`basename "$$n" .texi`; \
			d=`basename "$$b" .tely`; \
			if [ -e "$$n" ] ; then \
				$(EXTRACT_TEXI_FILENAMES) \
				-I $(top-src-dir)/Documentation \
				-I $(top-src-dir)/Documentation/"$$l" \
				-I $(top-src-dir)/Documentation/"$$r""$$d" \
				--known-missing-files=$(top-src-dir)/scripts/build/website-known-missing-files.txt \
			  $(quiet-flag) \
				-I $(OUT) -o $(OUT) "$$n" ; \
			fi ; \
		done; \
	done;

website-bibs: website-version
	BSTINPUTS=$(top-src-dir)/Documentation/web \
		$(WEB_BIBS) -s web \
		-s $(top-src-dir)/Documentation/lily-bib \
		-o $(OUT)/others-did.itexi \
		$(quiet-flag) \
		$(top-src-dir)/Documentation/web/others-did.bib
	BSTINPUTS=$(top-src-dir)/Documentation/web \
		$(WEB_BIBS) -s web \
		-s $(top-src-dir)/Documentation/lily-bib \
		-o $(OUT)/we-wrote.itexi \
		$(quiet-flag) \
		$(top-src-dir)/Documentation/web/we-wrote.bib


website-texinfo: website-version website-xrefs website-bibs
	for l in '' $(WEB_LANGS); do \
	        if test -n "$$l"; then \
			langopt=--lang="$$l"; \
			langsuf=.$$l; \
		fi; \
		$(TEXI2HTML) --prefix=index \
			--split=section \
			--I=$(top-src-dir)/Documentation/"$$l" \
			--I=$(top-src-dir)/Documentation \
			--I=$(OUT) \
			$$langopt \
			--init-file=$(texi2html-init-file) \
			-D web_version \
			--output=$(OUT)/"$$l" \
			$(top-src-dir)/Documentation/"$$l"/web.texi ; \
		ls $(OUT)/$$l/*.html | xargs grep -L 'UNTRANSLATED NODE: IGNORE ME' | sed 's!$(OUT)/'$$l'/!!g' | xargs $(MASS_LINK) --prepend-suffix="$$langsuf" hard $(OUT)/$$l/ $(OUT)/website/ ; \
	done


website-css:
	cp $(top-src-dir)/Documentation/css/*.css $(OUT)/website

website-pictures:
	mkdir -p $(OUT)/website/pictures
	cp $(PICTURES)/* $(OUT)/website/pictures
	ln -sf website/pictures $(OUT)/pictures

website-examples:
	mkdir -p $(OUT)/website/ly-examples
	cp $(EXAMPLES)/* $(OUT)/website/ly-examples

web-post:
	$(WEB_POST) $(OUT)/website

.PHONY: check-setup
check-setup:
ifeq ($(LILYPOND_WEB_MEDIA_GIT),)
	echo "Need a $LILYPOND_WEB_MEDIA_GIT environment variable!"
	exit 1
endif

website: check-setup website-texinfo website-css website-pictures website-examples web-post
	cp $(SERVER_FILES)/favicon.ico $(OUT)/website
	cp $(SERVER_FILES)/robots.txt $(OUT)/website
	cp $(top-htaccess) $(OUT)/.htaccess
	cp $(dir-htaccess) $(OUT)/website/.htaccess


