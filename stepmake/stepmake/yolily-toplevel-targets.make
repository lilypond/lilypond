# -*-Makefile-*-
# title	   Yolily_Toplevel_targets.make

local-dist: do-top-doc

$(package-icon):
	$(MAKE) -C Documentation/pictures icon

do-top-doc:
	-$(MAKE) -C Documentation/topdocs/ README_TOP_FILES="$(README_TXT_FILES)" copy-to-top

$(README_TXT_FILES): do-top-doc


htmldoc: 
	$(MAKE) CONFIGSUFFIX='www' local-WWW
	$(MAKE) CONFIGSUFFIX='www' -C Documentation WWW
	rm -f `find . -name \*.html~ -print`
	find `find Documentation -type d -name 'out-www'` -not -name '*dvi' -not -name '*ly' -not -name '*tex' -not -name '*.ps' -not -name 'out-www' > wwwlist
	-ln -f $(patch-dir)/$(distname).diff.gz out-www
	-ln -f $(depth)/$(distname).diff.gz out-www
	tar cfz $(outdir)/htmldoc.tar.gz  `cat wwwlist` `ls *.png out-www/$(distname).diff.gz $(ERRORLOG)`  index.html


# if you fix this, please fix yodl too!
check-top-web:
	$(MAKE) -C Documentation/topdocs WWW

