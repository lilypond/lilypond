local-dist: configure

local-distclean:
	rm -f config.hh config.make Makefile GNUmakefile \
		config.cache config.status config.log index.html \
		stepmake/stepmake/stepmake stepmake/stepmake/bin
	rm -rf autom4te.cache

local-maintainerclean:
	rm -f configure

GNUmakefile: GNUmakefile.in
	$(MAKE) INFILE=$< OUTFILE=$@ -f $(stepdir)/automatically-generated.sub.make

ifneq ($(PACKAGE),STEPMAKE)
aclocal.m4: $(stepmake)/aclocal.m4
	$(MAKE) INFILE=$< OUTFILE=$@ LINECOMMENT=dnl -f $(stepdir)/automatically-generated.sub.make

autogen.sh: $(stepmake)/autogen.sh
	$(MAKE) INFILE=$< OUTFILE=$@ LINECOMMENT=\# -f $(stepdir)/automatically-generated.sub.make
	chmod +x autogen.sh
endif


$(package-icon):
	$(MAKE) -C Documentation/pictures icon


do-top-doc:
	-$(MAKE) -C Documentation/topdocs/ README_TOP_FILES="$(TOPDOC_FILES)" make-txt-files


$(README_TXT_FILES): do-top-doc

local-clean:

install:
	$(LOOP)
ifeq ($(strip $(SRCMAKE)),)
	$(MAKE) final-install
endif

local-dist: do-top-doc

dist:
	rm -rf $(distdir)
	$(MAKE) local-dist $(distdir)
	chmod -R a+r $(distdir)
	chmod  a+x `find $(distdir) -type d -print`
	(cd ./$(depth)/$(outdir); $(TAR) -cf -  --owner=0 --group=0 $(DIST_NAME) | gzip -9 > $(DIST_NAME).tar.gz)
	rm -rf $(distdir)/

local-help:
	@echo -e "\
  web-uninstall  remove installed documentation with images\n\
  config      rerun configure\n\
  dist        roll tarball: $(depth)/$(outdir)/$(distname).tar.gz\n\
  distclean   also remove configure output\n\
  doc         update all documentation (without images)\n\
  cvs-clean   also remove out directories and generated files\n\
  maintainerclean also remove distributed generated files\n\
  po          make new translation Portable Object database\n\
  po-replace  do po-update and replace catalogs with msgmerged versions\n\
  po-update   update translation Portable Object database\n\
\n\
Some more targets are available for testing changes:\n\
  test-baseline  check  test-redo  test-clean\n\
For more information on these targets, see Testing LilyPond\n\
in Application Usage documentation.\n"

