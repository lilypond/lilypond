#
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
#

# identify module:
#
NAME = lilypond
MAJOR_VERSION = $(TOPLEVEL_MAJOR_VERSION)
MINOR_VERSION = $(TOPLEVEL_MINOR_VERSION)
PATCH_LEVEL = $(TOPLEVEL_PATCH_LEVEL)
# use to send patches, always empty for released version:
MY_PATCH_LEVEL = $(TOPLEVEL_MY_PATCH_LEVEL)
build = $(lily-dir)/$(outdir)/.build
#

# descent order into subdirectories:
#
SUBDIRS = include
#

# to be remade each build:
#
VERSION_DEPENDENCY = $(lily-version)
#

# list of c++ header files:
# 
HHFILES := $(wildcard *.hh) #$(shell ls *.hh $(ERROR_LOG))
#

# list of c++ source files:
#
CCFILES := $(wildcard *.cc) #$(shell ls *.cc $(ERROR_LOG))
#

# list of other source files:
#
EXTRA_SOURCE_FILES = $(shell ls *.y *.l $(ERROR_LOG))
#

# list of distribution files:
#
DISTFILES = .version Makefile $(HHFILES) $(CCFILES) $(EXTRA_SOURCE_FILES)
#

# list of custom libraries:
#
# yes, i know about the -L and -l options,
# but these libraries get rebuild when needed.
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

# explicit dependencies: (how to do auto?)
#
# ugh
#$(outdir)/version.cc: check-flower-version $(lily-version)


mylexer.cc: $(outdir)/parser.hh # sic
lexer.l: $(outdir)/parser.hh
#

# list of depend files:
#
DEPFILES = $(shell ls $(depdir)/*.dep $(ERROR_LOG))
#

# auto dependencies:
#
-include /dev/null $(DEPFILES)
#

