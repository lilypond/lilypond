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
NAME =flower-test
# include ./$(depth)/$(NAME)/.version
MODULE_NAME = flower
include ./$(depth)/flower/.version

#

# generic variables:
#
include ./$(depth)/make/Variables.make
include ./$(depth)/make/Files.make 
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
BUILDSTRINGTEST=$(MAINTARGET)
EXECSTRINGTEST=$(EXECUTABLE)

# don't do the exec, as this might fail if flowerlib isn't installed yet.
default:  $(BUILDSTRINGTEST) 


do-stringtest:
	$(EXECSTRINGTEST) > $(outdir)/result
	cmp $(outdir)/result result

dummy:

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make


# auto dependencies:
#
ifdef DEPFILES
include $(DEPFILES)
endif
#

