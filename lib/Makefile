#
# project  LilyPond -- the musical typesetter
# title	   makefile for micro-lily-lib
# file	   lib/Makefile 
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
NAME = lily
MAJOR_VERSION = $(TOPLEVEL_MAJOR_VERSION)
MINOR_VERSION = $(TOPLEVEL_MINOR_VERSION)
PATCH_LEVEL = $(TOPLEVEL_PATCH_LEVEL)
# use to send patches, always empty for released version:
MY_PATCH_LEVEL = $(TOPLEVEL_MY_PATCH_LEVEL)
build = $(lily-dir)/.build
#

# descent order into subdirectories:
#
SUBDIRS = include
#

# to be remade each build:
#
VERSION_DEPENDENCY =#
#

# list of c++ header files:
# 
HHFILES = $(shell ls *.hh $(ERROR_LOG))
#

# list of c++ source files:
#
CCFILES = $(shell ls *.cc $(ERROR_LOG))
#

# list of other source files:
#
EXTRA_SOURCE_FILES = # $(shell ls *.y *.l $(ERROR_LOG))
#

# list of distribution files:
#
DISTFILES = Makefile $(HHFILES) $(CCFILES) $(EXTRA_SOURCE_FILES)
#

# list of custom libraries:
#
# yes, i know about the -L and -l options,
# but these libraries get rebuild when needed.
CUSTOMLIBES = \

LOADLIBES +=
#

# main target of this module:
# 
# MAINTARGET = $(EXECUTABLE)
# MAINTARGET = $(LIBRARY)
# MAINTARGET = $(bindir)/$(EXECUTABLE)# huh?
MAINTARGET = $(libdir)/$(LIBRARY)# huh?

default: $(MAINTARGET)
#

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

# list of depend files:
#
DEPFILES = $(shell ls $(depdir)/*.dep $(ERROR_LOG))
#

# auto dependencies:
#
-include $(DEPFILES)
#

