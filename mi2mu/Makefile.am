# Automatically generated from ./mi2mu/Makefile.am.wild by autowild
# Do not edit
# Generated automatically by wild-perl 0.1
# project  LilyPond -- the musical typesetter
# title	   makefile for mi2mu
# file	   mi2mu/Makefile.am.wild
#
# Copyright (c) 1997 by
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>

SUBDIRS = include

bin_PROGRAMS = mi2mu

mi2mu_SOURCES = main.cc mi2mu-version.cc midi-parser.cc midi-score-parser.cc midi-track-parser.cc mudela-column.cc mudela-item.cc mudela-score.cc mudela-staff.cc mudela-stream.cc mudela-voice.cc template9.cc version.cc

# ugh -lfl bo lib/FlexLexer stuff
#mi2mu_LDADD = -llily -lflower -lfl

# mi2mu_LDFLAGS = -L$(absolute_builddir)/lib/.libs -L$(absolute_builddir)/flower/.libs
mi2mu_LDADD = $(top_builddir)/lib/liblily.la $(top_builddir)/flower/libflower.la @LEXLIB@
 

INCLUDES = -I$(srcdir)/include -I$(srcdir)/../lib/include -I$(srcdir)/../flower/include

EXTRA_DIST = Makefile.am.wild GNUmakefile VERSION TODO

# override default target for all-am: to get an extra dependency
all-am: Makefile $(PROGRAMS) wild-check

# dummy target for non-GNU makes; is overridden in GNUmakefile
wild-check:

distclean-local:
	-rm -rf .deps
$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl --top-srcdir="$(real_topdir)" < $< > $@