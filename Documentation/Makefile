# Documentation/Makefile

# subdir level:
#
depth = ..
#

# identify module:
#
NAME = Documentation
SUBDIRS=man

#

# generic variables:
#
include ./$(depth)/make/Variables.make 
include ./$(depth)/make/Version.make
include ./$(depth)/make/Files.make 
include ./$(depth)/make/Docrules.make
#


#
DOCFILES = $(wildcard *.doc)
OUTPODFILES = $(addprefix $(outdir)/,$(PODFILES))
BIBFILES= $(wildcard *.bib)
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


EXTRA_DISTFILES = $(XPMS)  $(DOCFILES)  vocabulary-data vocabulary-forms.el fonts.tex automake.urgh $(BIBFILES)

# don't do DVI files. They can only be made if lily is installed
do-doc: $(TEXTFILES)

html: $(HTMLFILES)

dvi: $(DVIFILES)

# generic targets and rules:
#
include $(depth)/make/Targets.make
include $(depth)/make/Rules.make
