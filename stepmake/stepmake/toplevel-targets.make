# -*-Makefile-*-
# title	   Toplevel_targets.make

local-dist: configure 

local-distclean: 
	rm -f config.h config.hh config.make Makefile GNUmakefile \
		config.cache config.status config.log index.html \
		afm tfm cmtfm share/locale/*/LC_MESSAGES/lilypond.mo

local-maintainerclean:
	rm -f configure

GNUmakefile: make/toplevel.make.in
	$(MAKE) INFILE=$< OUTFILE=$@ -f $(stepdir)/automatically-generated.sub.make

ifneq ($(PACKAGE),STEPMAKE)
aclocal.m4: $(stepmake)/aclocal.m4
	$(MAKE) INFILE=$< OUTFILE=$@ LINECOMMENT=dnl -f $(stepdir)/automatically-generated.sub.make
endif

local-WWW: #index.html 

index.html: check-top-web NEWS
	$(sed-version) < Documentation/topdocs/$(outdir)/topweb.html > $@
	$(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) --index=Documentation/top-docs/out-www/index.html $@

WWW-clean: local-WWW-clean
	$(MAKE) CONFIGSUFFIX='www' clean

local-WWW-clean:

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

local-help:

