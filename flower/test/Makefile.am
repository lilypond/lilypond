# Automatically generated from ./flower/test/Makefile.am.wild by autowild
# Do not edit
# Generated automatically by wild-perl 0.1
#
# project  LilyPond -- the musical typesetter
# title	   makefile for flowertest
# file	   flower/test/Makefile.am.wild
#
# Copyright (c) 1997 by
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>
#

# the wild-perl script needs this.
real_topdir = $(top_srcdir)/..

# bin_PROGRAMS = test

# TEST = test

# can't link to libflower: has not been created yet...
# test_SOURCES = main.cc mat-test.cc pqtest.cc rattest.cc stringtest.cc

EXTRA_SOURCES = main.cc mat-test.cc pqtest.cc rattest.cc stringtest.cc

EXTRA_DIST = result Makefile.am.wild $(EXTRA_SOURCES)

noinst_HEADERS = flower-test.hh

INCLUDES = -I../include

$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl --top-srcdir="$(real_topdir)" < $< > $@