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


# list of distribution files:
#
EXTRA_DISTFILES = Configure_variables.make.in lilypond.spec.in Toplevel.make.in
#

# generic variables:
#
include ./$(depth)/make/Variables.make
include ./$(depth)/make/Files.make 
#

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

lilypond.spec: lilypond.spec.in $(depth)/.version
	sed 's/@TOPLEVEL_VERSION@/${TOPLEVEL_VERSION}/g'< $< > $@
