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
include ./$(depth)/flower/.version
include ./$(depth)/.version
build = ./$(depth)/flower/lib/.build
#

# generic variables:
#
include ./$(depth)/make/Variables.make 
#

# descent order into subdirectories:
#
SUBDIRS = lib
#

# list of distribution files:
#
SCRIPTS = # make_version make_patch genheader clearlily configure
README_FILES = NEWS README TODO
DISTFILES= Makefile .version $(README_FILES) $(SCRIPTS)
#

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
# include ./$(depth)/make/Rules.make
#

# version:
#
flower-version: $(flower-version)
$(flower-version): ./$(depth)/flower/.version ./$(bindir)/make_fversion $(build)
	./$(bindir)/make_fversion "$(MAJOR_VERSION)" "$(MINOR_VERSION)" "$(PATCH_LEVEL)" "$(MY_PATCH_LEVEL)" "$(BUILD)" "$(CXX) $(CXXVER)" > $@
#

