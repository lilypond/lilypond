# -*-Makefile-*-
# title	   Toplevel_targets.make

include $(stepdir)/www-targets.make

local-dist: configure 

# urg!: this gets into every package (ie: yodl)
local-distclean: 
	rm -f config.h config.hh config.make Makefile GNUmakefile \
		config.cache config.status config.log index.html \
		afm tfm cmtfm share/locale/*/LC_MESSAGES/lilypond.mo

local-maintainerclean:
	rm -f configure

GNUmakefile: GNUmakefile.in
	$(MAKE) INFILE=$< OUTFILE=$@ -f $(stepdir)/automatically-generated.sub.make

ifneq ($(PACKAGE),STEPMAKE)
aclocal.m4: $(stepmake)/aclocal.m4
	$(MAKE) INFILE=$< OUTFILE=$@ LINECOMMENT=dnl -f $(stepdir)/automatically-generated.sub.make
endif


$(package-icon):
	$(MAKE) -C Documentation/pictures icon


do-top-doc:
	-$(MAKE) -C Documentation/topdocs/ README_TOP_FILES="$(README_TXT_FILES)" copy-to-top


$(README_TXT_FILES): do-top-doc

local-clean:

###check-top-web:
###	$(MAKE) -C Documentation/topdocs WWW
#####

####index.html: check-top-web NEWS

local-dist: do-top-doc

dist:
	rm -rf $(distdir)
	$(MAKE) local-dist $(distdir)
	chmod -R a+r $(distdir)
	chmod  a+x `find $(distdir) -type d -print` 
	(cd ./$(depth)/$(outdir); $(TAR) cf - $(DIST_NAME) | gzip -9 > $(DIST_NAME).tar.gz)
# ugh.
# Can't compare "stage1" dist with "stage2" dist in this way?
	-ln -f $(depth)/$(outdir)/$(distname).tar.gz $(release-dir)
	rm -rf $(distdir)/

#
#
local-help:
	@echo -e "\
  config      rerun configure\n\
  deb         build Debian package\n\
  diff        generate patch: $(depth)/$(outdir)/$(distname).diff.gz\n\
  .           Options:\n\
  .             from=0.1.74\n\
  .             help==\n\
  .             release==\n\
  .             to=0.1.74.jcn2\n\
  dist        roll tarball: $(depth)/$(outdir)/$(distname).tar.gz\n\
  distclean   cleaner than clean (duh)\n\
  doc         update all documentation\n\
  release     roll tarball and generate patch\n\
  rpm         build Red Hat package\n\
  po          make new translation Portable Object database\n\
  po-replace  do po-update and replace catalogs with msgmerged versions\n\
  po-update   update translation Portable Object database\n\
  web         update website in out-www\n\
  web-clean   clean out-www\n\
\n\
Some of these top level targets (diff, dist, release) can be issued\n\
from anywhere in the source tree.\n\
"\
#
