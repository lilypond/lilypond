# Documentation/Makefile

# subdir level:
#
depth = ..
#

# identify module:
#
NAME = Documentation
MAJOR_VERSION = $(TOPLEVEL_MAJOR_VERSION)
MINOR_VERSION = $(TOPLEVEL_MINOR_VERSION)
PATCH_LEVEL = $(TOPLEVEL_PATCH_LEVEL)
# use to send patches, always empty for released version:
MY_PATCH_LEVEL = $(TOPLEVEL_MY_PATCH_LEVEL)
build = ./$(depth)/lily/.build
#

# generic variables:
#
include ./$(depth)/make/Variables.make 
#

# list of source files:
#
PODFILES = $(shell ls *.pod)
OUTPODFILES = $(patsubst %,$(outdir)/%,$(PODFILES))
TEXTFILES = $(OUTPODFILES:.pod=.text)
MANFILES = $(OUTPODFILES:.pod=.1)
HTMLFILES = $(OUTPODFILES:.pod=.html)
#

# list of distribution files:
#
DISTFILES = $(PODFILES) Makefile lelie_logo.png
#

default: do-doc

do-doc: $(TEXTFILES)

html: $(pod)
	pod2html

htmldist: html
	./$(bindir)/make_website

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

