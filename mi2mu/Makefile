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
#

# identify module:
#
NAME = mi2mu
MODULE_NAME = mi2mu
include $(mi2mu-dir)/.version
build = $(mi2mu-dir)/.build
#

# descent order into subdirectories:
#
SUBDIRS = include
#

# to be remade each build:
#
VERSION_DEPENDENCY = $(mi2mu-version)
#

# list of c++ header files:
# 
HHFILES = $(shell ls *.hh $(ERROR_LOG))
#

# list of c++ source files:
#
CCFILES = $(shell ls *.cc $(ERROR_LOG))
#

# list of other source files:
#
EXTRA_SOURCE_FILES = $(shell ls *.y *.l $(ERROR_LOG))
#

# list of distribution files:
#
DISTFILES = Makefile .version $(HHFILES) $(CCFILES) $(EXTRA_SOURCE_FILES)
#

# list of custom libraries:
#
# yes, i know about the -L and -l options,
# but these libraries get rebuild when needed.
CUSTOMLIBES = $(LIBLILY) $(LIBFLOWER) 

LOADLIBES +=
#

# main target of this module:
#
# MAINTARGET = $(EXECUTABLE)
# MAINTARGET = $(LIBRARY)
MAINTARGET = $(bindir)/$(EXECUTABLE)# huh?
# MAINTARGET = $(libdir)/$(LIBRARY)# huh?

default: $(MAINTARGET)
#

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

# explicit dependencies: (how to do auto?)
#
midi-lexer.l:	$(outdir)/midi-parser.hh
version.cc:	check-flower-version $(mi2mu-version)
#

# version:
#
mi2mu-version: $(mi2mu-dir)/$(genout) $(mi2mu-version)
$(mi2mu-version): $(mi2mu-dir)/$(genout) $(mi2mu-dir)/.version ./$(bindir)/make_version $(build)
	./$(bindir)/make_version "$(MAJOR_VERSION)" "$(MINOR_VERSION)" "$(PATCH_LEVEL)" "$(MY_PATCH_LEVEL)" "$(BUILD)" "$(CXX) $(CXXVER)" > $@
#

# list of depend files:
#
DEPFILES = $(shell ls $(depdir)/*.dep $(ERROR_LOG))
#

# auto dependencies:
#
-include $(DEPFILES)
#

