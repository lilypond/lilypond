# Documentation/Makefile

# subdir level:
#
depth = ..
#

# identify module:
#
NAME = Documentation
SUBDIRS=man tex

#

# generic variables:
#
include ./$(depth)/make/Variables.make 
include ./$(depth)/make/Version.make
include ./$(depth)/make/Files.make 
include ./$(depth)/make/Docrules.make
#


#
TEXTFILES = $(OUTPODFILES:.pod=.txt)
GROFFFILES = $(OUTPODFILES:.pod=.1)
HTMLFILES = $(OUTPODFILES:.pod=.html)


default: all do-doc

#
# list of distribution files:
#
XPMS=$(wildcard *.xpm)
giffiles=$(XPMS:.xpm=.gif)

gifs: $(addprefix $(outdir)/, $(giffiles))


EXTRA_DISTFILES = $(XPMS)   vocabulary-data vocabulary-forms.el  automake.urgh

# don't do DVI files. They can only be made if lily is installed
do-doc: $(TEXTFILES)

local-WWW: $(HTMLFILES)

# generic targets and rules:
#
include $(depth)/make/Targets.make
include $(depth)/make/Rules.make
