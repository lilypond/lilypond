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
#

# generic variables:
#
include Stable.make
include ./$(depth)/make/Variables.make
include VERSION
#

# descent order into subdirectories:
#
SUBDIRS = include
#


# list of distribution files:
#
EXTRA_DISTFILES = VERSION 
#

# list of custom libraries:
#
MODULE_LIBDEPS= check-flower-deps check-lib-deps 
MODULE_LIBES= -llily -lflower

#

# main target of this module:
#

default: $(EXECUTABLE)

out/engraver.o: include/engraver.hh
#
include ./$(depth)/make/Files.make 

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

# force these: Make can't know these have to be generated in advance
my-lily-lexer.cc: $(outdir)/parser.hh
$(outdir)/lexer.cc: $(outdir)/parser.hh

#


localclean:
	rm -f $(outdir)/parser.* $(outdir)/lexer.cc

#should merge with mi2mu rule
localinstall: installexe

localuninstall: uninstallexe

$(outdir)/lily-version.o: $(outdir)/version.hh

# make config.hh. Ugh
$(outdir)/main.o: $(libout)/config.hh

$(libout)/config.hh: check-lib-deps
