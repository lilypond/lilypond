# Generated automatically by wild-perl 0.1
# project  LilyPond -- the musical typesetter
# title	   makefile for flowertest
# file	   flowertest/Makefile.am.wild

bin_PROGRAMS = test

TESTS = $(top_srcdir)/flowertest/check.sh

test_SOURCES = main.cc mat-test.cc pqtest.cc rattest.cc stringtest.cc

test_LDADD = $(top_builddir)/flower/libflower.la @LEXLIB@

EXTRA_DIST = result Makefile.am.wild GNUmakefile check.sh

noinst_HEADERS = flower-test.hh

INCLUDES = -I$(top_srcdir)/flower/include

# override default target for all-am: to get an extra dependency
all-am: Makefile $(PROGRAMS) wild-check

# dummy target for non-GNU makes; is overridden in GNUmakefile
wild-check:

$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@
