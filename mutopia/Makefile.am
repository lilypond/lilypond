# Generated automatically by wild-perl 0.1# mutopia/Makefile.am.wild

SUBDIRS = J.S.Bach

LYFILES = gallina.ly standchen-16.ly standchen-20.ly standchen.ly standje.ly
TEXFILES = gallina.tex standchen-16.tex standchen-20.tex
M4FILES = dummy.m4
EXTRA_DIST = Makefile.am.wild TODO $(LYFILES) $(TEXFILES) $(M4FILES)

OUTFILES = $(M4FILES:%.m4=%)

default: all $(OUTFILES)
$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@