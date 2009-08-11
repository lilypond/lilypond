.PHONY : all clean bin-clean config default dist exe help html lib TAGS\
	 po doc doc-stage-1 WWW-1 WWW-2 WWW-post local-WWW-1 local-WWW-2

all:	 default
	$(LOOP)

man:
	$(LOOP)

clean: local-clean
	-rm -rf "./$(outdir)"
	$(LOOP)

ifeq (,$(findstring metafont,$(STEPMAKE_TEMPLATES)))
bin-clean: local-bin-clean
	-rm -rf "./$(outdir)"
	$(LOOP)
else
bin-clean:
endif

local-bin-clean: local-clean

ifneq ($(strip $(depth)),.)
dist:
	$(MAKE) -C $(depth) dist
endif

distclean: clean
	$(LOOP)
	$(MAKE) local-distclean

cvs-clean:
	$(MAKE) local-distclean
	rm -rf out
	rm -rf out-www
	rm -f aclocal.m4 configure

maintainerclean:
	$(LOOP)
	$(MAKE)	local-maintainerclean
	$(MAKE) local-distclean


# This doesn't allow command-line options, is it really useful? -jm
config:
	./$(src-depth)/configure


generic-help:
	@echo -e "\
Makefile for $(PACKAGE_NAME) $(TOPLEVEL_VERSION)\n\
Usage: make ["VARIABLE=value"]... [TARGET]\n\
\n\
Targets specific to current directory:\n"

help: generic-help local-help
	@echo -e "Generic targets:\n\
  all *       update everything except documentation with images\n\
  clean *     remove all generated stuff in $(outdir)\n\
  bin-clean * same as clean, except that mf/out is preserved\n\
  default     same as the empty target\n\
  doc *       update documentation with images in directory \`out-www'\n\
  doc-stage-1 * update only PDF and Info documentation in directory \`out-www'\n\
  doc-clean * clean \`out-www' directory\n\
  exe         update all executables\n\
  help        this help\n\
  install *   install programs and data (prefix=$(prefix))\n\
  lib         update all libraries\n\
  uninstall*  uninstall programs and data\n\
  TAGS        generate tagfiles\n\
\n\
\`make' may be invoked from any subdirectory.\n\
Note that all commands marked with a star (*) recurse into subdirectories;\n\
prepend \`local-' to restrict operation to the current directory.\n\
Example: \`local-clean'."

local-help:

local-dist: $(DIST_FILES) $(OUT_DIST_FILES) $(NON_ESSENTIAL_DIST_FILES)
	mkdir -p $(distdir)/$(localdir)
	$(LN) $(DIST_FILES:%=$(src-dir)/%) $(distdir)/$(localdir)

	case "$(NON_ESSENTIAL_DIST_FILES)x" in x) ;; *) \
		$(LN) $(NON_ESSENTIAL_DIST_FILES:%=$(src-dir)/%) $(distdir)/$(localdir);; \
	esac
	case "$(OUT_DIST_FILES)x" in x) ;; *) \
		mkdir -p $(distdir)/$(localdir)/$(outdir); \
		$(LN) $(OUT_DIST_FILES) $(distdir)/$(localdir)/$(outdir);; \
	esac
	$(foreach i, $(SUBDIRS), $(MAKE) top-src-dir=$(top-src-dir) distdir=$(distdir) localdir=$(localdir)/$(notdir $(i)) -C $(i) local-dist &&) true



html: $(HTML_FILES)

TAGS:
	$(LOOP)
	$(MAKE) local-tags

DEEPER_TAGS_FILES = $(shell find $(pwd) -mindepth 2 -name 'TAGS')
local-tags:
	-if [ -n "$(TAGS_HEADERS)$(TAGS_SOURCES)$(DEEPER_TAGS_FILES)" ]; then \
		etags $(ETAGS_FLAGS) $(DEEPER_TAGS_FILES:%=--include=%) \
			$(TAGS_SOURCES) $(TAGS_HEADERS) $(ERROR_LOG) ; \
		ctags $(CTAGS_FLAGS) $(TAGS_SOURCES) $(TAGS_HEADERS) \
			$(ERROR_LOG) ; \
	fi

$(outdir)/version.hh: $(depth)/VERSION $(config_make) $(step-bindir)/make-version.py
	$(PYTHON) $(step-bindir)/make-version.py $< > $@

$(outdir)/config.hh: $(config_h)
	cp -p $< $@

configure: configure.in aclocal.m4
	NOCONFIGURE=yes $(src-depth)/autogen.sh
	chmod +x configure

local-clean:

local-distclean:

local-maintainerclean:

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

installextradoc:
	-$(INSTALLPY) -d $(DESTDIR)$(prefix)/doc/$(package)
	cp -r $(EXTRA_DOC_FILES) $(prefix)/doc/$(package)

-include $(outdir)/dummy.dep $(wildcard $(outdir)/*.dep)

$(outdir)/dummy.dep:
	-mkdir -p $(outdir)
	touch $(outdir)/dummy.dep
	echo '*' > $(outdir)/.gitignore

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
local-WWW-1:
local-WWW-2:
WWW-post:

WWW-1: local-WWW-1
	$(LOOP)

WWW-2: local-WWW-2
	$(LOOP)
endif

doc: doc-stage-1
	$(MAKE) out=www WWW-2
	$(MAKE) out=www WWW-post

local-doc:
	$(MAKE) out=www local-WWW-1
	$(MAKE) out=www local-WWW-2
	$(MAKE) out=www WWW-post

doc-stage-1:
	$(MAKE) -C $(depth)/scripts/build out=
	$(MAKE) out=www WWW-1

doc-clean:
	$(MAKE) out=www clean
