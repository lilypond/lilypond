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
DATAFILES = $(wildcard *.data)
TEXTFILES = $(OUTPODFILES:.pod=$(DOTTEXT))
GROFFFILES = $(OUTPODFILES:.pod=.1)
HTMLFILES = $(OUTPODFILES:.pod=.html) $(datafiles)

default: do-doc

#
# list of distribution files:
#
XPMS=$(wildcard *.xpm)
giffiles = $(addprefix $(outdir)/,$(XPMS:.xpm=.gif))

gifs: $(giffiles)

datafiles = $(addprefix $(outdir)/,$(DATAFILES:.data=.html))

EXTRA_DISTFILES = $(XPMS) vocabulary.data vocabulary-forms.el  automake.urgh\
	gnu-music-history

# don't do DVI files. They can only be made if lily is installed
do-doc: $(TEXTFILES)

readme-topfiles:
	cd $(depth); for i in $(README_TOPFILES); do \
	ln -f $$i Documentation/$(outdir)/$$i$(DOTTEXT); done

README_TOPFILES=NEWS DEDICATION TODO ANNOUNCE-0.1
README_TXTFILES=$(addprefix $(outdir)/,$(addsuffix $(DOTTEXT), $(README_TOPFILES)))

local-WWW: $(HTMLFILES) readme-topfiles $(README_TXTFILES) $(giffiles)

# generic targets and rules:
#
include $(depth)/make/Targets.make
include $(depth)/make/Rules.make

# ugh
check-doc-deps: do-doc
	@echo # hi

doc: do-doc

