# Generated automatically by wild-perl 0.1
# title	   top level makefile for LilyPond  
# file	   Makefile.am.wild

SUBDIRS = bin flower flowertest lib lily mf mi2mu debian\
 	Documentation init input tex make mutopia

SCHRIFTEN = configure configure.in install-sh aclocal.m4
README_FILES = BUGS DEDICATION ANNOUNCE-0.1 ANNOUNCE \
 COPYING ONEWS NEWS README TODO \
 INSTALL AUTHORS PATCHES
EXTRA_DIST = Makefile.am.wild GNUmakefile .dstreamrc mudela-mode.el acinclude.m4 VERSION $(README_FILES) $(SCHRIFTEN)


INSTALL:
	rm -f INSTALL
	ln `$(FIND) ./ -name INSTALL.txt -print | head -1` $@

AUTHORS:
	rm -f AUTHORS
	ln `$(FIND) ./ -name AUTHORS.txt -print | head -1` $@

PATCHES:
	rm -f PATCHES
	ln `$(FIND) ./ -name PATCHES.txt -print | head -1` $@


# a hack due to a bug in automake: the macro AM_CONFIG_HEADER does not
# respect the directory
distclean-local:
	rm -f flower/config.hh

# override default target for all-am: to get an extra dependency
all-am: Makefile $(PROGRAMS) wild-check

# dummy target for non-GNU makes; is overridden in GNUmakefile
wild-check:

# dummy target for non-GNU makes; is overridden in GNUmakefile
make-in-build:

$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@