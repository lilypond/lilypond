# THIS IS A TEMPLATE FOR SUB-PROJECT MAKEFILES
# should we make Include-dir and Stuff-dir templates too?
#
# project  LilyPond -- the musical typesetter
# title	   makefile for ...
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
#

# identify module:
#
NAME = ...
MODULE_NAME = 
# include ./$(depth)/$(NAME)/.version
MAJOR_VERSION = 0
MINOR_VERSION = 0
PATCH_LEVEL = 0
# use to send patches, always empty for released version:
MY_PATCH_LEVEL = # include separator: "-1" or ".a"
build = $(lily-dir)/$(outdir)/.build #????!
#

# descent order into subdirectories:
#
SUBDIRS =
#

# to be remade each build:
#
VERSION_DEPENDENCY = $(lily-version)
#

# module compile settings: (not generally needed!
#
EXTRA_CFLAGS =
EXTRA_CXXFLAGS =
EXTRA_LDFLAGS =
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
EXTRA_SOURCE_FILES = $(shell ls *.y *.l $(ERROR_LOG))
#

# list of distribution files:
#
DISTFILES = Makefile $(HHFILES) $(CCFILES) $(EXTRA_SOURCE_FILES)
#

# list of custom libraries:
#
CUSTOMLIBES = \

LOADLIBES +=
#

# main target of this module:
#
MAINTARGET = $(EXECUTABLE)
# MAINTARGET = $(LIBRARY)

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

