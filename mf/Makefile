#
# project  LilyPond -- the musical typesetter
# title	   makefile for yet to add in MF files
# file	   ../Makefile 
#
# Copyright (c) 1997 by
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>
#

# subdir level:
#
depth = ..
#

# generic variables:
#
include ./$(depth)/make/Variables.make 

include ./$(depth)/.version

# descent order into subdirectories:
#
SUBDIRS =
#

#
include ./$(depth)/make/Files.make
MFFILES:=$(wildcard *.mf)

EXTRA_DISTFILES = $(MFFILES) README


# list of custom libraries:
#
CUSTOMLIBES = \

LOADLIBES +=
#

# main target of this module:
#
MAINTARGET = 

default: $(MAINTARGET)
#

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

lily-mfdir = $(MFDIR)/lilypond

localinstall:
	$(INSTALL) -d $(lily-mfdir)
	$(INSTALL) -m 755 $(MFFILES) $(lily-mfdir)

localuninstall:
	for i in $(MFFILES); do rm -f $(lily-mfdir)/$$i; done
	-rmdir $(lily-mfdir)
