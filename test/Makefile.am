# Generated automatically by wild-perl 0.1
# project  LilyPond -- the musical typesetter
# title	   makefile for test
# file	   test/Makefile.am.wild

TEST_SCHRIFTEN = check-lily-midi.sh check-lily-paper.sh check-mi2mu.sh

# ugh ugh
TESTS = $(top_srcdir)/test/check-lily-midi.sh $(top_srcdir)/test/check-lily-paper.sh $(top_srcdir)/test/check-mi2mu.sh

INPUTS = test.midi

RESULTS = ok.midi ok.tex ok.ly

EXTRA_DIST = Makefile.am.wild GNUmakefile $(TEST_SCHRIFTEN) $(INPUTS) $(RESULTS)

# override default target for all-am: to get an extra dependency
all-am: Makefile $(PROGRAMS) wild-check

# dummy target for non-GNU makes; is overridden in GNUmakefile
wild-check:

$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@
