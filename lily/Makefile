# project  LilyPond -- the musical typesetter
# title	   makefile for lilypond
# file	   lily/Makefile 
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
include ./$(depth)/make/Files.make 
include .version

#

# identify module:
#
NAME = lilypond

#

# descent order into subdirectories:
#
SUBDIRS = include
#

# to be remade each build:
#
VERSION_DEPENDENCY = $(lily-version)
#


# list of distribution files:
#
DISTFILES = .version Makefile $(ALL_SOURCES)
#

# list of custom libraries:
#
# yes, i know about the -L and -l options,
# but these libraries get rebuilt when needed.
CUSTOMLIBES = $(LIBLILY) $(LIBFLOWER)

LOADLIBES +=
#

# main target of this module:
#
# MAINTARGET = $(EXECUTABLE)
# MAINTARGET = $(LIBRARY)
MAINTARGET = $(lily_bindir)/$(EXECUTABLE)# huh?
# MAINTARGET = $(libdir)/$(LIBRARY)# huh?

default: $(MAINTARGET)
#

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

# force these: Make can't know these have to be generated in advance
my-lily-lexer.cc: $(outdir)/parser.hh
$(outdir)/lexer.cc: $(outdir)/parser.hh

#

# list of depend files:
#
DEPFILES = $(wildcard $(depdir)/*.dep)
#

# auto dependencies:
#
-include /dev/null $(DEPFILES)
#

