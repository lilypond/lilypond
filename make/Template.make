# THIS IS A TEMPLATE FOR SUB-PROJECT MAKEFILES
# should we make Include-dir and Stuff-dir templates too?

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

# identify module:
#
NAME = ...
# include ./$(depth)/$(NAME)/.version
MAJOR_VERSION = 0
MINOR_VERSION = 0
PATCH_LEVEL = 0
# use to send patches, always empty for released version:
MY_PATCH_LEVEL = # include separator: "-1" or ".a"
build = ./$(depth)/lily/.build
#

# generic variables:
#
include ./$(depth)/make/Variables.make 
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
HHFILES = $(shell ls *.hh)
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
DISTFILES = $(HHFILES) $(CCFILES) $(EXTRA_SOURCE_FILES)
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

# auto dependencies:
#
include ./$(outdir)/*.dep
#

