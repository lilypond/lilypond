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
depth = ../..
#

# identify module:
#
NAME = flower
MODULE_NAME = flower
include ./$(depth)/flower/.version
build = ./$(depth)/flower/lib/.build
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
VERSION_DEPENDENCY = include/fversion.hh
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
EXTRA_SOURCE_FILES =# $(shell ls *.y *.l)
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

# sic.
$(include-flower)/flower-config.hh:
	touch $@
 
# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

# auto dependencies:
#
-include ./$(outdir)/*.dep
#

