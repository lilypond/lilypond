# Documentation/Makefile

# subdir level:
#
depth = ..
#

# identify module:
#
NAME = Documentation

#

# generic variables:
#
include ./$(depth)/make/Variables.make 
include ./$(depth)/make/Version.make
include ./$(depth)/make/Files.make 
#

#

OUTPODFILES = $(addprefix $(outdir)/,$(PODFILES))
TEXTFILES = $(OUTPODFILES:.pod=.text)
GROFFFILES = $(OUTPODFILES:.pod=.1)
HTMLFILES = $(OUTPODFILES:.pod=.html)
#

# list of distribution files:
#
EXTRA_DISTFILES = lelie_icon.gif lelie_logo.gif
#

default: do-doc
	true
do-doc: $(TEXTFILES)

html: $(HTMLFILES)

htmldist: html
	./$(lily_bindir)/make_website


# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

localclean:
	rm -f out/*
	rm -f $(TEXTFILES) $(HTMLFILES) $(GROFFFILES)

MAN1FILES = lilypond convert-mudela mi2mu
MAN1GROFF = $(addprefix $(outdir)/, $(addsuffix .1,$(MAN1FILES)))

bla:
	echo $(MANGROFF)
	echo $(MANFILES)

localinstall: $(outdir)/lilypond.1 $(outdir)/mudela.5
	$(INSTALL) -d $(mandir)/man5
	$(INSTALL) -d $(mandir)/man1
	$(INSTALL) -m 755 $(MAN1GROFF) $(mandir)/man1
	$(INSTALL) -m 755 $(outdir)/mudela.5 $(mandir)/man5

localuninstall:
	rm -f $(mandir)/man1/lilypond.1
	rm -f $(mandir)/man1/convert-mudela.1
