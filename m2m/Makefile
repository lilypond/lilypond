#
# project  LilyPond -- the musical typesetter
# title	   makefile for m2m
# file	   m2m/Makefile 
#
# Copyright (c) 1997 by
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>
#

# subdir level:
#
depth = ..
#

# identify module:
#
NAME = m2m
MODULE_NAME = m2m
MAJOR_VERSION = $(TOPLEVEL_MAJOR_VERSION)
MINOR_VERSION = $(TOPLEVEL_MINOR_VERSION)
PATCH_LEVEL = $(TOPLEVEL_PATCH_LEVEL)
# use to send patches, always empty for released version:
MY_PATCH_LEVEL = $(TOPLEVEL_MY_PATCH_LEVEL)
# include ./$(depth)/m2m/.version
build = ./$(depth)/lily/.build
#

# generic variables:
#
include ./$(depth)/make/Variables.make 
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
HHFILES = # $(shell ls include/*.hh)
#

# list of c++ source files:
#
CCFILES = $(shell ls *.cc)
#

# list of other source files:
#
EXTRA_SOURCE_FILES = $(shell ls *.y *.l)
#

# list of distribution files:
#
DISTFILES = Makefile .version $(HHFILES) $(CCFILES) $(EXTRA_SOURCE_FILES)
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
MAINTARGET = $(bindir)/$(EXECUTABLE)# huh?
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
midi-lexer.l:	$(outdir)/midi-parser.hh
midi-main.cc:	check-flower-version $(lily-version)
#

# auto dependencies:
#
-include ./$(outdir)/*.dep
#

