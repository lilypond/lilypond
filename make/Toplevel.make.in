# -*-Makefile-*-
########################################################
# project  LilyPond -- the musical typesetter
# title	   top level makefile for LilyPond  
# file	   Makefile 
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>

# subdir level:
#
depth = .
#

# identify module:
#
NAME = lilypond
SUBDIRS = bin flower lib lily mf mi2mu debian\
	Documentation init input tex make mutopia
include VERSION
#

# descent order into subdirectories:
#

# list of distribution files:
#
SCRIPTS = configure configure.in aclocal.m4
README_FILES = BUGS DEDICATION ANNOUNCE-0.1 ANNOUNCE \
 COPYING ONEWS NEWS README TODO \
 INSTALL.txt AUTHORS.txt PATCHES.txt
EXTRA_DISTFILES = config.make.in config.hh.in .dstreamrc mudela-mode.el VERSION $(README_FILES) $(SCRIPTS) 

# do not dist ./Makefile (is copied from make/Toplevel.make)
DISTFILES:=$(EXTRA_DISTFILES)
#


# generic targets and rules:
#
include ./$(depth)/make/Version.make
include ./$(depth)/make/Variables.make 
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make

localdist: configure

local-distclean: 
	rm -f config.hh config.make Makefile config.cache config.status config.log

Makefile: make/Toplevel.make.in
	chmod +w $@
	echo '# WARNING WARNING WARNING WARNING' > $@
	echo '# do not edit! this is generated from make/Toplevel.make.in' >> $@
	cat $< >> $@
	chmod -w $@

