#
# project  LilyPond -- the musical typesetter
# title	   makefile for mi2mu
# file	   mi2mu/Makefile 
#
# Copyright (c) 1997 by
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>
#

# subdir level:
#
depth = ..
#

NAME = mi2mu
MODULE_NAME = mi2mu

# generic variables:
#
include ./$(depth)/make/Variables.make
include ./$(depth)/make/Files.make 
#

# identify module:
#
include $(mi2mu-dir)/VERSION
build = $(mi2mu-dir)/$(outdir)/.build
#

# descent order into subdirectories:
#
SUBDIRS = include
#

# to be remade each build:
#
VERSION_DEPENDENCY = $(mi2mu-version)
#


# list of distribution files:
#
EXTRA_DISTFILES = VERSION TODO
#

# list of custom libraries:
#
MODULE_LIBDEPS=check-flower-deps check-lib-deps
MODULE_LIBES=-llily -lflower
#


default: $(EXECUTABLE)

#

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

# explicit dependencies: (how to do auto?)
#
midi-lexer.l:	$(outdir)/midi-parser.hh

$(outdir)/mi2mu-version.o: $(outdir)/version.hh

EXECUTABLES=mi2mu

include $(depth)/make/Executable.make
