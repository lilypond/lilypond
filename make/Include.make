#
# project  LilyPond -- the musical typesetter
# title	   generic red tape for include/Makefile
# file	   make/Include.make
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>

# identify module:
#
NAME = generic-include
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

# list of c++ header files:
# 
HHFILES = $(shell ls *.hh)
#

# list of c++ inline files:
# 
INLFILES = $(shell ls *.inl)
#

# list of c++ template files:
# 
TCCFILES = $(shell ls *.tcc)
#

# list of distribution files:
#
DISTFILES = Makefile $(HHFILES) $(INLFILES) $(TCCFILES)
#

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

