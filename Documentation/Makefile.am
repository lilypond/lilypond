# Generated automatically by wild-perl 0.1
# Documentation/Makefile.am.wild

DOCFILES = introduction.doc mudela-course.doc mudela-man.doc
PODFILES = AUTHORS.pod CodingStyle.pod INSTALL.pod MANIFESTO.pod PATCHES.pod convert-mudela.pod faq.pod gnu-music.pod index.pod internals.pod language.pod lilypond.pod links.pod literature.pod ly2dvi.pod mi2mu.pod mudela-book.pod mutopia.pod other-packages.pod
OUTPODFILES = $(PODFILES)
TEXTFILES = $(OUTPODFILES:.pod=.txt)
GROFFFILES = $(OUTPODFILES:.pod=.1)
HTMLFILES = $(OUTPODFILES:.pod=.html)
DVIFILES = $(DOCFILES:.doc=.dvi)

XPMS = lelie_icon.xpm lelie_logo.xpm lelieblond.xpm
giffiles = $(XPMS:.xpm=.gif)

EXTRA_DIST = Makefile.am.wild Rules.make $(XPMS) $(DOCFILES) $(PODFILES) vocabulary-data vocabulary-forms.el 

MAN1FILES = lilypond convert-mudela mi2mu mudela-book ly2dvi
MAN1GROFF = $(addsuffix .1,$(MAN1FILES))

default: all $(TEXTFILES)

%.1: %.pod
	$(POD2MAN) --section=1 $*.pod > $@

outdir = .

install-data-hook: $(MAN1GROFF)
	$(INSTALL) -d $(mandir)/man1
	$(INSTALL) -m 644 $(MAN1GROFF) $(mandir)/man1

uninstall-hook:
	(cd  $(mandir)/man1/; rm -f $(MAN1GROFF))

html: $(HTMLFILES)

clean-local:
	-rm $(TEXTFILES) $(MAN1GROFF)

include $(top_srcdir)/Documentation/Rules.make
$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@