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
TEXTFILES = $(OUTPODFILES:.pod=.txt)
GROFFFILES = $(OUTPODFILES:.pod=.1)
HTMLFILES = $(OUTPODFILES:.pod=.html)
DVIFILES = $(addprefix $(outdir)/, $(DOCFILES:.doc=.dvi))


default: do-doc
#
# list of distribution files:
#
XPMS=$(wildcard *.xpm)
giffiles=$(XPMS:.xpm=.gif)

gifs: $(addprefix $(outdir)/, $(giffiles))


EXTRA_DISTFILES = $(XPMS)  $(DOCFILES)  vocabulary-data vocabulary-forms.el 

# don't do DVI files. They can only be made if lily is installed
do-doc: $(TEXTFILES)

html: $(HTMLFILES)

htmldist: html
	$(lily_bindir)/make_website

dvi: $(DVIFILES)

# generic targets and rules:
#
include $(depth)/make/Targets.make
include $(depth)/make/Rules.make
#

localclean:
	rm -f `find $(outdir) -type f`



MAN1FILES = lilypond convert-mudela mi2mu mudela-book
MAN1GROFF = $(addprefix $(outdir)/, $(addsuffix .1,$(MAN1FILES)))

localinstall: $(outdir)/lilypond.1
	$(INSTALL) -d $(mandir)/man1
	$(INSTALL) -m 644 $(MAN1GROFF) $(mandir)/man1


localuninstall:
	(cd  $(mandir)/man1/; rm -f $(MAN1GROFF))



