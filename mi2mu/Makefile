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

# generic variables:
#
include ./$(depth)/make/Variables.make
include ./$(depth)/make/Files.make 
#

# identify module:
#
NAME = mi2mu
MODULE_NAME = mi2mu
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
MODULE_LIBDEPS=check-flower-deps check-lily-deps
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

# list of depend files:
#
DEPFILES = $(wildcard $(depdir)/*.dep)
#

# auto dependencies:
#
-include /dev/null $(DEPFILES)
#
localclean:
	rm -f $(outdir)/{midi-parser,midi-lexer}.*

localinstall: installexe

localuninstall: uninstallexe


$(outdir)/mi2mu-version.o: $(outdir)/version.hh
