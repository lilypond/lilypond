#
# project  LilyPond -- the musical typesetter
# title	   zucht
# file	   make/Makefile 
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>
#		...your sort order here, or how to comment-out a comment

# subdir level:
#
depth = ..
#

# identify module:
#
NAME = make
MAJOR_VERSION = $(TOPLEVEL_MAJOR_VERSION)
MINOR_VERSION = $(TOPLEVEL_MINOR_VERSION)
PATCH_LEVEL = $(TOPLEVEL_PATCH_LEVEL)
# use to send patches, always empty for released version:
MY_PATCH_LEVEL = $(TOPLEVEL_MY_PATCH_LEVEL)
build = ./$(depth)/lily/$(outdir)/.build
#

# list of generic make files:
#
MAKEFILES = $(shell ls *.make)
#

# list of distribution files:
#
DISTFILES = Configure_variables.make.in Makefile lilypond.spec.in $(MAKEFILES)
#

# generic variables:
#
include ./$(depth)/make/Variables.make 
#

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

lilypond.spec: lilypond.spec.in
	sed 's/@TOPLEVEL_VERSION@/${TOPLEVEL_VERSION}/g'< $< > $@