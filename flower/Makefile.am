# Automatically generated from ./flower/Makefile.am.wild by autowild
# Do not edit
# Generated automatically by wild-perl 0.1
# project  FlowerSoft C++ library
# title	   top level makefile for FlowerLib
# file	   flower/Makefile.am.wild
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>
#		...your sort order here, or how to comment-out a comment

SUBDIRS = include test 

# the wild-perl script needs this.
real_topdir = $(top_srcdir)/..

lib_LTLIBRARIES = libflower.la

README_FILES = ONEWS NEWS README TODO VERSION
EXTRA_DIST = Makefile.am.wild aclocal.m4 configure configure.in $(README_FILES)

libflower_la_SOURCES = choleski.cc cpu-timer.cc data-file.cc diagonal-storage.cc dictionary.cc directed-graph.cc dstream.cc flower-debug.cc flower-version.cc full-storage.cc interval.cc lgetopt.cc libc-extension.cc matrix-debug.cc matrix-storage.cc matrix.cc nscalar.cc path.cc rational.cc scalar.cc string-convert.cc string.cc stringutil.cc text-db.cc text-stream.cc unionfind.cc vector.cc

libflower_la_LDFLAGS = -version-info @CURRENT@:@REVISION@:@AGE@

INCLUDES = -I$(srcdir)/include

distclean-local:
	-rm -rf .deps
$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl --top-srcdir="$(real_topdir)" < $< > $@