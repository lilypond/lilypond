# Generated automatically by wild-perl 0.1
# input/Makefile.am.wild

LYFILES = abbrev.ly beam-bug.ly beams.ly cadenza.ly collisions.ly coriolan-alto.ly denneboom.ly font-body.ly font.ly font11.ly font13.ly font16.ly font20.ly font26.ly gourlay.ly keys.ly kortjakje.ly multi.ly pedal.ly praeludium-fuga-E.ly rhythm.ly scales.ly scripts.ly sleur.ly slurs.ly spacing.ly stem.ly test-lyrics.ly tril.ly twinkle-pop.ly twinkle.ly
TEXFILES = dummy.tex
M4FILES = dummy.m4
EXTRA_DIST = Makefile.am.wild TODO $(LYFILES) $(TEXFILES) $(M4FILES)

OUTFILES = $(M4FILES:%.m4=%)

default: all $(OUTFILES)

$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@
