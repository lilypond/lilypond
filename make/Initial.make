#
# project  LilyPond -- the musical typesetter
# title	   initial makefile for lilypond
# file	   make/Initial.make
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>
#

# subdir level:
#
depth = ..
#

# ugh
NAME = dummy 
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


# ugh
# initdefault: $(CCDIR)/parser.cc $(CCDIR)/lexer.cc hdr/version.hh alldeps
initdefault: check-flower-version $(lily-version) check-mi2mu-version dummydep
#	$(MAKE) -C ./$(depth)/lily $(outdir)/parser.cc

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

