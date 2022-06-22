.PHONY : all clean default dist exe help html lib man TAGS\
	 po doc WWW WWW-post local-WWW local-WWW\
	 local-all local-clean local-doc

all: default
	$(LOOP)

local-all: default

man:
	$(LOOP)

clean: local-clean
	-rm -rf $(outdir)
	$(LOOP)

ifneq ($(strip $(depth)),.)
dist:
	$(MAKE) -C $(depth) dist
endif

generic-help:
	@echo "Makefile for $(PACKAGE_NAME) $(TOPLEVEL_VERSION)"
	@echo "Usage: make ["VARIABLE=value"]... [TARGET]"
	@echo
	@echo "Targets specific to current directory:"

help: generic-help local-help
	@echo "Generic targets that recurse into subdirectories:"
	@echo "  all          update everything except documentation with images"
	@echo "               (same as the empty target)"
	@echo "  clean        remove all generated stuff in $(outdir)"
	@echo "  doc          update documentation with images in directory \`out-www'"
	@echo "  doc-clean    clean \`out-www' directory"
	@echo "  install      install programs and data (prefix=$(prefix))"
	@echo "  uninstall    uninstall programs and data"
	@echo "  test         build regression tests for the program and scripts"
	@echo
	@echo "  *Note: Prepend \`local-' (eg. \`local-clean') to restrict"
	@echo "         any of the above commands to the current directory."
	@echo
	@echo "Other generic targets:"
	@echo "  default      same as \`make all', but restricted to the current directory"
	@echo "  exe          update all executables"
	@echo "  help         this help"
	@echo "  lib          update all libraries"
	@echo "  TAGS         generate tagfiles"
	@echo
	@echo "\`make' may be invoked from any subdirectory that contains a GNUmakefile."

local-help:

html: $(HTML_FILES)

TAGS:
	$(LOOP)
	$(MAKE) local-tags

DEEPER_TAGS_FILES = $(shell find $(abs-src-dir) -mindepth 2 -name 'TAGS')
local-tags:
	-if [ -n "$(TAGS_HEADERS)$(TAGS_SOURCES)$(DEEPER_TAGS_FILES)" ]; then \
		cd $(abs-src-dir) ; \
		etags $(ETAGS_FLAGS) $(DEEPER_TAGS_FILES:%=--include=%) \
			$(TAGS_SOURCES) $(TAGS_HEADERS) $(ERROR_LOG) ; \
		ctags $(CTAGS_FLAGS) $(TAGS_SOURCES) $(TAGS_HEADERS) \
			$(ERROR_LOG) ; \
	fi

$(outdir)/version.hh: $(depth)/VERSION $(config_make) $(buildscript-dir)/create-version-hh.py
	$(PYTHON) $(buildscript-dir)/create-version-hh.py > $@

$(outdir)/config.hh: $(config_h)
	cp -p $< $@

configure: configure.ac aclocal.m4
	NOCONFIGURE=yes $(src-depth)/autogen.sh
	chmod +x configure

local-clean:

local-distclean:

install-strip:
	$(MAKE) INSTALLPY="$(INSTALLPY) -s" install

ifeq ($(strip $(depth)),.)
final-install:
else
final-install:
	$(LOOP)

install: local-install
	$(LOOP)
endif

local-install:

uninstall: local-uninstall
	$(LOOP)

local-uninstall:

# Create the output directory before any targets are built, except for
# "make clean" because that would be silly.
ifeq (,$(filter clean,$(MAKECMDGOALS)))
-include $(outdir)/dummy.dep
$(outdir)/dummy.dep:
	-mkdir -p $(outdir)
	touch $(outdir)/dummy.dep
	echo '*' > $(outdir)/.gitignore
endif

-include $(wildcard $(outdir)/*.dep)

check: local-check
	$(LOOP)

local-check:

# ugh.  ugh ugh ugh
$(config_make): $(top-src-dir)/configure
	@echo "************************************************************"
	@echo "configure changed! You should probably reconfigure manually."
	@echo "************************************************************"
	(cd $(top-build-dir); ./config.status)
	touch $@		# do something for multiple simultaneous configs.


#### Documentation (website and tarball)

# documentation is built in two stages,
# plus WWW-post (only at toplevel)
# see INSTALL for more information.

ifeq ($(out),www)
local-WWW:
WWW-post:

WWW: local-WWW
	$(LOOP)

endif

doc: doc-messages
	$(MAKE) out=www WWW
	$(MAKE) out=www WWW-post

local-doc:
	$(MAKE) out=www local-WWW
	$(MAKE) out=www WWW-post

doc-messages:
	$(MAKE) -C $(top-build-dir)/Documentation/po messages

doc-clean:
	$(MAKE) out=www clean
