#
# project  FlowerSoft C++ library
# title	   top level makefile for FlowerLib
# file	   flower/Makefile
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
NAME = flower
MODULE_NAME = flower
#
# edit .version only
include ./$(depth)/.version
include ./$(depth)/flower/.version

# generic variables:
#
include ./$(depth)/make/Variables.make
include ./$(depth)/make/Files.make 

#
CXXFLAGS+=-O2
# descent order into subdirectories:
#
SUBDIRS = include test 
#

# list of distribution files:
#
SCRIPTS = 
README_FILES = NEWS README TODO
EXTRA_DISTFILES= configure config.hh.in configure.in .version $(README_FILES) $(SCRIPTS)
#


# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

# version:
#

localdist: configure
