# Generated automatically by wild-perl 0.1# mutopia/J.S.Bach/Makefile.am.wild

LYFILES = preludes-1.ly preludes-2.ly preludes-4.ly preludes-5.ly preludes-6.ly scsii-menuetto.ly wtk1-fugue1.ly wtk1-fugue2.ly wtk1-prelude1.ly
TEXFILES = preludes.tex
M4FILES = preludes-3.ly.m4 wtk1-prelude2.ly.m4
EXTRA_DIST = Makefile.am.wild $(LYFILES) $(TEXFILES) $(M4FILES)

OUTFILES = $(M4FILES:%.m4=%)

default: all $(OUTFILES)

$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@