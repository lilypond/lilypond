#
# project  LilyPond -- the musical typesetter
# title	   top level makefile for LilyPond  
# file	   Makefile 
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>
#		...your sort order here, or how to comment-out a comment

# subdir level:
#
depth = .
#

# identify module:
#
NAME = lilypond

# edit in .version only!
MAJOR_VERSION = $(TOPLEVEL_MAJOR_VERSION)
MINOR_VERSION = $(TOPLEVEL_MINOR_VERSION)
PATCH_LEVEL = $(TOPLEVEL_PATCH_LEVEL)
# use to send patches, always empty for released version:
MY_PATCH_LEVEL = $(TOPLEVEL_MY_PATCH_LEVEL)
build = ./$(depth)/lily/.build
#

# generic variables:
#
include ./$(depth)/make/Variables.make 
#

# descent order into subdirectories:
#
SUBDIRS = flower lib lily mi2mu \
	Documentation bin init input tex make
#

# list of distribution files:
#
# SYMLINKS = # naah, configure
SCRIPTS = configure
README_FILES = ANNOUNCE COPYING INSTALL NEWS README TODO
DISTFILES= Makefile .dstreamrc .version $(README_FILES) $(SCRIPTS) $(SYMLINKS)
#

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

