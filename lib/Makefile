#
# project  LilyPond -- the musical typesetter
# title	   makefile for micro-lily-lib
# file	   lib/Makefile 
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
include ./$(depth)/make/Version.make 
#

# identify module:
#
NAME = lily
#

# descent order into subdirectories:
#
SUBDIRS = include
#

# to be remade each build:
#
VERSION_DEPENDENCY =#
#

# list of distribution files:
#
EXTRA_DISTFILES = config.hh.in
#

MODULE_CXXFLAGS += -D_REENTRANT

# list of custom libraries:

LOADLIBES +=
#

# main target of this module:
# 
# MAINTARGET = $(EXECUTABLE)
# MAINTARGET = $(LIBRARY)
# MAINTARGET = $(bindir)/$(EXECUTABLE)# huh?
MAINTARGET = $(outdir)/$(LIBRARY)# huh?

default: configheader $(MAINTARGET)
#

localclean:
	rm -f $(MAINTARGET)

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make

# auto dependencies:
#
-include $(DEPFILES)
#

.PHONY: configheader

configheader: $(outdir)/config.hh

# Ugh ugh
$(outdir)/config.hh: config.hh.in
	sed 's!@datadir@!$(datadir)!'<  $< > $@
