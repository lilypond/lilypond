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
include out/Flower-flags.make
#
# edit .version only
include ./$(depth)/.version
include ./$(depth)/flower/.version

# generic variables:
#
include ./$(depth)/make/Variables.make
include ./$(depth)/make/Files.make 


#
# descent order into subdirectories:
#
SUBDIRS = include test 
#

# list of distribution files:
#
SCRIPTS = 
README_FILES = NEWS README TODO
EXTRA_DISTFILES= configure config.hh.in configure.in .version $(README_FILES) $(SCRIPTS) Flower-flags.make.in
#


# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#
default: shared-lib

shared-lib: $(SHAREDLIBRARY) 

# version:
#

localdist: configure

-include /dev/null $(DEPFILES)
#

localdistclean:
	rm -f config.cache config.status

localinstall: $(LIBFLOWER)
	$(INSTALL) -d $(libdir)
	$(INSTALL) $(LIBFLOWER) $(libdir)
#	ln -s $(libdir)/libflower.so.$(VERSION) $(libdir

localuninstall:
	rm -f $(libdir)/libflower.{so,a}
