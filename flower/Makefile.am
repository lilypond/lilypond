# Generated automatically by wild-perl 0.1
# project  FlowerSoft C++ library
# title	   top level makefile for FlowerLib
# file	   flower/Makefile.am.wild
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>
#		...your sort order here, or how to comment-out a comment

SUBDIRS = include

lib_LTLIBRARIES = libflower.la

README_FILES = ONEWS NEWS README TODO VERSION
EXTRA_DIST = Makefile.am.wild GNUmakefile $(README_FILES)

libflower_la_SOURCES = choleski.cc cpu-timer.cc data-file.cc diagonal-storage.cc dictionary.cc directed-graph.cc dstream.cc flower-debug.cc flower-version.cc full-storage.cc interval.cc lgetopt.cc libc-extension.cc matrix-debug.cc matrix-storage.cc matrix.cc nscalar.cc path.cc rational.cc scalar.cc string-convert.cc string.cc stringutil.cc text-db.cc text-stream.cc unionfind.cc vector.cc

libflower_la_LDFLAGS = -version-info @CURRENT@:@REVISION@:@AGE@

INCLUDES = -I$(top_srcdir)/flower/include

distclean-local:
	-rm -rf .deps

# override default target for all-am: to get an extra dependency
all-am: Makefile $(PROGRAMS) wild-check

# dummy target for non-GNU makes; is overridden in GNUmakefile
wild-check:

# dummy target for non-GNU makes; is overridden in GNUmakefile
make-in-build:

$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@