# title	   generic make targets
# file	   make/Targets.make

.PHONY : all clean config default dist doc exe help html lib TAGS\
	 po

all:	 default
	$(LOOP)

man:
	$(LOOP)

# be careful about deletion.
clean: localclean
	-rm -f $(outdir)/*
	$(LOOP)

ifneq ($(strip $(depth)),.)
dist:
	make -C $(depth) dist
endif

distclean: clean 
	$(LOOP)
	$(MAKE) local-distclean

maintainerclean: 
	$(LOOP)
	$(MAKE)	local-maintainerclean
	$(MAKE) local-distclean


# configure:
#
config:
	./$(depth)/configure
#


# target help:
#
generic-help:
	@echo -e "\
Makefile for $(PACKAGE_NAME) $(TOPLEVEL_VERSION)\n\
Usage: $(MAKE) ["VARIABLE=value"]... [TARGET]\n\
\n\
Targets:\n"

help: generic-help local-help
	@echo -e "\
  all         update everything\n\
  clean       remove all genated stuff in $(oudir)\n\
  config      rerun configure\n\
  deb         build Debian package\n\
  default     same as the empty target\n\
  diff        generate patch: $(depth)/$(outdir)/$(distname).diff.gz\n\
  .           Options:\n\
  .             from=0.1.74\n\
  .             help==\n\
  .             release==\n\
  .             to=0.1.74.jcn2\n\
  dist        roll tarball: $(depth)/$(outdir)/$(distname).tar.gz\n\
  distclean   cleaner than clean (duh)\n\
  doc         update all documentation\n\
  exe         update all executables\n\
  help        this help\n\
  install     install programs and data (prefix=$(prefix))\n\
  lib         update all libraries\n\
  release     roll tarball and generate patch\n\
  rpm         build RedHat package\n\
  po          make new translation Portable Object database\n\
  po-update   update translation Portable Object database\n\
  tar         same as dist\n\
  TAGS        genarate tagfiles\n\
  zip         build binary doze distribution\n\
\n\
Make may be invoked from any subdirectory\n\
Note that all commands recurse into SUBSDIRS\n\
"\
#

local-help:

doc: local-doc
	$(LOOP)

local-doc:

local-dist: $(DIST_FILES) $(OUT_DIST_FILES) $(NON_ESSENTIAL_DIST_FILES)
	mkdir -p $(distdir)/$(localdir)
	$(LN) $(DIST_FILES) $(distdir)/$(localdir)

	case "$(NON_ESSENTIAL_DIST_FILES)x" in x) ;; *) \
		$(LN) $(NON_ESSENTIAL_DIST_FILES) $(distdir)/$(localdir);; \
	esac

	case "$(OUT_DIST_FILES)x" in x) ;; *) \
		mkdir -p $(distdir)/$(localdir)/out; \
		$(LN) $(OUT_DIST_FILES) $(distdir)/$(localdir)/out;; \
	esac
#	$(foreach i, $(SUBDIRS), $(MAKE) distdir=../$(distdir) localdir=$(localdir)/$(i) -C $(i) local-dist &&) true
# absolute for installed stepmake
	$(foreach i, $(SUBDIRS), $(MAKE) topdir=$(topdir) distdir=$(distdir) localdir=$(localdir)/$(notdir $(i)) -C $(i) local-dist &&) true



html: $(HTML_FILES)

TAGS:
	-if [ "$(TAGS_FILES)" != "" ]; then \
		etags -CT $(TAGS_FILES) || \
		ctags -h ".h.hh.tcc.icc" $(TAGS_FILES) $(ERROR_LOG); \
	fi

	$(LOOP)

# ugh . -> $(outdir)
$(outdir)/VERSION: $(depth)/VERSION
	cp -p $< $@

$(outdir)/version.hh: $(outdir)/VERSION
	$(PYTHON) $(step-bindir)/make-version.py $< > $@

$(outdir)/config.h: $(config_h)
	cp -p $< $@

# should this be in Rules?
configure: configure.in aclocal.m4
	autoconf 
	chmod +x configure

localclean:

local-distclean:

local-maintainerclean:

install-strip:
	$(MAKE) INSTALL="$(INSTALL) -s" install

install: localinstall
	$(LOOP)

localinstall:

uninstall: localuninstall
	$(LOOP)

localuninstall:

installextradoc:
	-$(INSTALL) -d $(prefix)/doc/$(package)
	$(foreach i, $(EXTRA_DOC_FILES),\
		cp -r $(i) $(prefix)/doc/$(package) &&) true

WWW: local-WWW
	$(LOOP)

local-WWW:

include $(stepdir)/package.make

include $(outdir)/dummy.dep $(DEP_FILES)

$(outdir)/dummy.dep:
	-mkdir -p $(outdir)
	touch $(outdir)/dummy.dep


check: local-check
	$(LOOP)

local-check:

# ugh.  ugh ugh ugh
$(depth)/$(configuration).make: $(depth)/configure
	@echo "**************************************"
	@echo "configure changed! You should probably reconfigure manually."
	@echo "**************************************"
	(cd $(depth); ./config.status)
	touch $@		# do something for multiple simultaneous configs.
