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
# edit VERSION only
include ./$(depth)/VERSION
include ./$(depth)/flower/VERSION

# generic variables:
#
include ./$(depth)/make/Variables.make
include ./$(depth)/make/Files.make 

#include $(flowerout)/Flower-flags.make


#
# descent order into subdirectories:
#
SUBDIRS = include 
#

# list of distribution files:
#
SCRIPTS = 
README_FILES = ONEWS NEWS README TODO
EXTRA_DISTFILES= VERSION $(README_FILES) $(SCRIPTS)
#Flower-flags.make.in
# aclocal.m4 configure config.hh.in configure.in




# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

default: the-lib

THE_LIB=$(outdir)/$(LIB_PREFIX)flower$(LIB_SUFFIX)
the-lib: $(THE_LIB) 

# version:
#

localdist: 

-include /dev/null $(DEPFILES)
#

localdistclean:
	rm -f config.cache config.status config.log

localinstall: $(LIBFLOWER)
ifeq ($(LIB_SUFFIX),.so)
	$(INSTALL) -d $(libdir)
	$(INSTALL) $(LIBFLOWER).$(VERSION) $(libdir)
	ln -s $(LIB_PREFIX)flower$(LIB_SUFFIX).$(VERSION) $(libdir)/$(LIB_PREFIX)flower$(LIB_SUFFIX).$(MAJOR_VERSION)
	ln -s $(LIB_PREFIX)flower$(LIB_SUFFIX).$(VERSION) $(libdir)/$(LIB_PREFIX)flower$(LIB_SUFFIX)
endif

localuninstall:
	rm -f $(libdir)/libflower.{so*,a}


$(outdir)/flower-version.o: $(outdir)/version.hh
