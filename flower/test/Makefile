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

#

# generic variables:
#
include ./$(depth)/make/Variables.make
include ./$(depth)/make/Files.make 
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


# list of distribution files:
#
EXTRA_DISTFILES = result 

# list of custom libraries:
#

MODULE_LIBDEPS=check-flower-deps
MODULE_LIBES = -lflower
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
	$(EXECSTRINGTEST) > $(outdir)/result    # should cmp with a 'standard result'
	cmp $(outdir)/result result

dummy:

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

# list of depend files:
#
DEPFILES = $(wildcard $(depdir)/*.dep )
#

# auto dependencies:
#
-include $(DEPFILES)
#

