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
initdefault: check-flower-version $(lily-version) dummydep
#	$(MAKE) -C ./$(depth)/lily $(outdir)/parser.cc

# ugh!
dummydep: 
	touch ./$(depth)/flower/lib/$(depdir)/dummy.dep
	touch ./$(depth)/lib/$(depdir)/dummy.dep
	touch ./$(depth)/lily/$(depdir)/dummy.dep
	touch ./$(depth)/m2m/$(depdir)/dummy.dep

#$(DEPDIR)/%.dep:  $(CCDIR)/%.cc
#	$(DODEP) $(CXX) -E  $(CXXFLAGS) $^ > /dev/null

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

