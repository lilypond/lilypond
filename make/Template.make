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

# descent order into subdirectories:
#
SUBDIRS =
#

# module compile settings: (not generally needed!)
#
EXTRA_CFLAGS =
EXTRA_CXXFLAGS =
EXTRA_LDFLAGS =
#
include ./$(depth)/make/Files.make

# list of extra distribution files:
# Makefile, C++ and pod are dist'ed automatically
EXTRA_DISTFILES = 

# list of custom libraries:
#
CUSTOMLIBES = \

LOADLIBES +=
#

# main target of this module:
#
# MAINTARGET = $(EXECUTABLE)
# MAINTARGET = $(LIBRARY)
MAINTARGET = 

default: $(MAINTARGET)
#

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#


