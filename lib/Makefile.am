# Generated automatically by wild-perl 0.1
# project  LilyPond -- the musical typesetter
# title	   makefile for micro-lily-lib
# file	   lib/Makefile 
#
# Copyright (c) 1997 by
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>
#

SUBDIRS = include

lib_LTLIBRARIES = liblily.la

liblily_la_SOURCES = binary-source-file.cc duration-convert.cc duration.cc input.cc mapped-file-storage.cc moment.cc plet.cc simple-file-storage.cc source-file.cc source.cc template.cc warn.cc windhoos-suck-suck-suck-thank-you-cygnus.cc

INCLUDES = -I$(top_srcdir)/lib/include -I$(top_srcdir)/flower/include
MODULE_CXXFLAGS += -D_REENTRANT

EXTRA_DIST = Makefile.am.wild GNUmakefile

distclean-local:
	-rm -rf .deps
$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@
