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
include ./Rules.make
#


#
DOCFILES = $(wildcard *.doc)
OUTPODFILES = $(addprefix $(outdir)/,$(PODFILES))
TEXTFILES = $(OUTPODFILES:.pod=.text)
GROFFFILES = $(OUTPODFILES:.pod=.1)
HTMLFILES = $(OUTPODFILES:.pod=.html)
DVIFILES = $(addprefix $(outdir)/, $(DOCFILES:.doc=.dvi))

#
# list of distribution files:
#
EXTRA_DISTFILES = lelie_icon.gif lelie_logo.gif  $(DOCFILES)
default: do-doc

# don't do DVI files. They can only be made if lily is installed
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

MAN1FILES = lilypond convert-mudela mi2mu mudela-book
MAN1GROFF = $(addprefix $(outdir)/, $(addsuffix .1,$(MAN1FILES)))

localinstall: $(outdir)/lilypond.1 $(outdir)/mudela.5
	$(INSTALL) -d $(mandir)/man5
	$(INSTALL) -d $(mandir)/man1
	$(INSTALL) -m 755 $(MAN1GROFF) $(mandir)/man1


localuninstall:
	(cd  $(mandir)/man1/; rm -f $(MAN1GROFF))



