# Generated automatically by wild-perl 0.1
# mi2mu/include/Makefile.am.wild

noinst_HEADERS = mi2mu-global.hh mi2mu-proto.hh midi-parser.hh midi-score-parser.hh midi-track-parser.hh mudela-column.hh mudela-item.hh mudela-score.hh mudela-staff.hh mudela-stream.hh mudela-voice.hh dummy.icc dummy.tcc

EXTRA_DIST = Makefile.am.wild GNUmakefile

# override default target for all-am: to get an extra dependency
# all-am: Makefile $(PROGRAMS) wild-check
# ugh, automake doesn't generate the all-am target here...
all: Makefile $(HEADERS) wild-check

# dummy target for non-GNU makes; is overridden in GNUmakefile
wild-check:

$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@
