# title	   generic make targets
# file	   make/Targets.make

.PHONY : all clean config default dist doc doc++  exe help html lib TAGS\
	 po

# target all:
#
all:	 default
	$(LOOP)


# be careful about deletion.
clean: localclean
	-rm -f $(outdir)/*
	$(LOOP)

distclean: clean 
	$(LOOP)
	$(MAKE) local-distclean

maintainerclean: 
	$(LOOP)
	$(MAKE)	local-maintainerclean
	$(MAKE) local-distclean

include $(stepdir)/Po.make


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
  diff        generate patch: $(package)-$(TOPLEVEL_VERSION).diff.gz\n\
  .           Options:\n\
  .             from=0.1.74\n\
  .             help==\n\
  .             release==\n\
  .             to=0.1.74.jcn2\n\
  distclean   cleaner than clean (duh)\n\
  doc         update all documentation\n\
  doc++       make doc++ documentation\n\
  exe         update all executables\n\
  help        this help\n\
  install     install programs and data (prefix=$(prefix))\n\
  lib         update all libraries\n\
  release     roll tarball and generate patch\n\
  rpm         build RedHat package\n\
  tar         same as dist\n\
  TAGS        genarate tagfiles\n\
  zip         build binary doze distribution\n\
\n\
Make may be invoked from any subdirectory\n\
Note that all commands recurse into SUBSDIRS\n\
"\
#

local-help:

doc:
#	$(MAKE) -C $(depth)/Documentation do-doc
	$(MAKE) -C $(depth)/Documentation all


# Ugh.  C++ specific.
doc++:
	(cd $(outdir); sh ../$(step-bindir)/tar-docxx.sh $(package)-$(TOPLEVEL_VERSION).tar.gz)



check-state-vector:
	if [ "`tail -1 $(state-vector)`" != "$(TOPLEVEL_VERSION)" ]; then\
	  echo $(TOPLEVEL_VERSION) >> $(state-vector); \
	fi


local-dist: $(DIST_FILES) $(OUT_DIST_FILES) $(NON_ESSENTIAL_DIST_FILES)
	mkdir $(distdir)/$(localdir)
	$(LN) $(DIST_FILES) $(distdir)/$(localdir)

#UGH UGH . make ifdef doesn't mix with string substitution semantics (late expansion vs. early expansion)
# 
	if [ "$(NON_ESSENTIAL_DIST_FILES)x" != "x" ] ; then \
		$(LN) $(NON_ESSENTIAL_DIST_FILES) $(distdir)/$(localdir); \
	fi

	if [ "$(OUT_DIST_FILES)x" != "x" ] ; then \
		mkdir $(distdir)/$(localdir)/out; \
		$(LN) $(OUT_DIST_FILES) $(distdir)/$(localdir)/out; \
	fi
	$(foreach i, $(SUBDIRS), $(MAKE) distdir=../$(distdir) localdir=$(localdir)/$(i) -C $(i) local-dist &&) true



html: $(HTML_FILES)

TAGS:
	-if [ "$(TAGS_FILES)" != "" ]; then \
		etags -CT $(TAGS_FILES) || \
		ctags -h ".h.hh.tcc.icc" $(TAGS_FILES) $(ERROR_LOG); \
	fi

	$(LOOP)

$(outdir)/version.hh: VERSION
	sh ./$(step-bindir)/make-version.sh > $@


# should this be in Rules?
configure: configure.in aclocal.m4
	autoconf - < $<> $@
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

include $(stepdir)/Package.make
include $(make-dir)/Targets.make

include $(outdir)/dummy.dep $(DEP_FILES)

$(outdir)/dummy.dep:
	-mkdir $(outdir)
	touch $(outdir)/dummy.dep


check: local-check
	$(LOOP)

local-check:
