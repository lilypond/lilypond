# project  LilyPond -- the musical typesetter
# title	   makefile for lilypond
# file	   lily/Makefile 
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
NAME = lilypond
SUBDIRS = include


# generic variables:
include Stable.make
include ./$(depth)/make/Variables.make
include VERSION
# list of distribution files:
#
EXTRA_DISTFILES = VERSION 
MODULE_LIBDEPS= check-flower-deps check-lib-deps 
MODULE_LIBES= -llily -lflower

# main target of this module:
#
default: $(EXECUTABLE)
include ./$(depth)/make/Files.make 
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make


# force these: Make can't know these have to be generated in advance
my-lily-lexer.cc: $(outdir)/parser.hh
$(outdir)/lexer.cc: $(outdir)/parser.hh


EXECUTABLES=lilypond
include $(depth)/make/Executable.make

$(outdir)/lily-version.o: $(outdir)/version.hh

