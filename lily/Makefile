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

# generic variables:
#
include Stable.make
include ./$(depth)/make/Variables.make
include ./$(depth)/make/Files.make 
include .version
#

# identify module:
#
NAME = lilypond

#

# descent order into subdirectories:
#
SUBDIRS = include
#


# list of distribution files:
#
EXTRA_DISTFILES = .version 
#

# list of custom libraries:
#
MODULE_LIBDEPS= check-flower-deps check-lily-deps
MODULE_LIBES= -llily -lflower

#

# main target of this module:
#
default: $(EXECUTABLE)
#

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

# force these: Make can't know these have to be generated in advance
my-lily-lexer.cc: $(outdir)/parser.hh
$(outdir)/lexer.cc: $(outdir)/parser.hh

#

# list of depend files:
#
DEPFILES = $(wildcard $(depdir)/*.dep)
#

# auto dependencies:
#
-include /dev/null $(DEPFILES)
#


localclean:
	rm -f $(outdir)/parser.* $(outdir)/lexer.cc

#should merge with mi2mu rule
localinstall: installexe

localuninstall: uninstallexe
