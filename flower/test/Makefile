#
# project  LilyPond -- the musical typesetter
# title	   makefile for flowertest
# file	   test/Makefile 
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
NAME =stringtest
# include ./$(depth)/$(NAME)/.version
MODULE_NAME = flower
include ./$(depth)/flower/.version
build = ./$(depth)/flower/test/$(outdir)/.build
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
VERSION_DEPENDENCY = #
#

# module compile settings: (not generally needed!
#
EXTRA_CFLAGS = -DSTRING_TEST
EXTRA_CXXFLAGS =
EXTRA_LDFLAGS =
#

# list of c++ header files:
# 
# HHFILES = $(shell ls *.hh $(ERROR_LOG))
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
CUSTOMLIBES = $(LIBFLOWER)\

LOADLIBES +=
#


# main target of this module:
#
# MAINTARGET = $(EXECUTABLE)
# MAINTARGET = $(LIBRARY)
MAINTARGET = $(lily_bindir)/$(EXECUTABLE)# huh?
# MAINTARGET = $(libdir)/$(LIBRARY)# huh?

#default: $(MAINTARGET)
BUILDSTRINGTEST=$(MAINTARGET)
EXECSTRINGTEST=$(EXECUTABLE)
default:  $(BUILDSTRINGTEST) do-stringtest
do-stringtest:
	$(EXECSTRINGTEST)    # should cmp with a 'standard result'

dummy:

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

