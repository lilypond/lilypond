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
giffiles = $(addprefix $(outdir)/,$(XPMS:.xpm=.gif))

gifs: $(giffiles)

EXTRA_DISTFILES = $(XPMS)   vocabulary-data vocabulary-forms.el  automake.urgh\
	gnu-music-history

# don't do DVI files. They can only be made if lily is installed
do-doc: $(TEXTFILES)


$(outdir)/%.txt: $(depth)/%
	-rm $@
	ln $< $@

README_TOPFILES=NEWS DEDICATION TODO ANNOUNCE-0.1
README_TXTFILES=$(addprefix $(outdir)/,$(addsuffix .txt, $(README_TOPFILES)))

local-WWW: $(HTMLFILES) $(README_TXTFILES)  $(giffiles)

# generic targets and rules:
#
include $(depth)/make/Targets.make
include $(depth)/make/Rules.make

