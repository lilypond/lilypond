# -*-Makefile-*-
# title	   Toplevel_targets.make

local-dist: configure 

local-distclean: 
	rm -f config.hh config.make Makefile config.cache \
		config.status config.log index.html

local-maintainerclean:
	rm -f configure

GNUmakefile: make/toplevel.make.in
	chmod +w $@
	echo "# WARNING WARNING WARNING WARNING" > $@
	echo "# do not edit! this is generated from make/Toplevel.make.in" >> $@
	cat $< >> $@
	chmod -w $@

aclocal.m4: $(stepdir)/../aclocal.m4
	cp $< $@

local-WWW: #index.html 

index.html: check-top-web NEWS
	$(sed-version) < Documentation/topdocs/$(outdir)/topweb.html > $@
	$(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) --index=Documentation/top-docs/out-www/index.html $@

WWW-clean:
	$(MAKE) CONFIGSUFFIX='www' clean

dist:	
	rm -rf $(distdir)
	$(MAKE) local-dist $(distdir)
	chmod -R a+r $(distdir)
	chmod  a+x `find $(distdir) -type d -print` 
	(cd ./$(depth)/$(outdir); $(TAR) cf - $(DIST_NAME) | gzip -9 > $(DIST_NAME).tar.gz)
# ugh.
# Can't compare "stage1" dist with "stage2" dist in this way?
	-ln -f $(depth)/$(outdir)/$(distname).tar.gz $(release-dir)
	rm -rf ./$(distdir)/

local-help:
	@echo -e "\
  dist        roll tarball: $(outdir)/$(package)-$(TOPLEVEL_VERSION).tar.gz\n"


