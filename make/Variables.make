#
# project  LilyPond -- the musical typesetter
# title	   generic variables
# file	   make/Variables.make
# abstract 
#
# do not change this file for site-wide extensions; please use 
# make/$(OUTDIR_NAME)/Site.make; 
#
# Any change in files in this directory (make/) would be distributed, if 
# you do make dist 
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>

# toplevel version info, might be handy?
#
include $(depth)/VERSION


ifeq (0,${MAKELEVEL})

# Don't try to outsmart us, you puny computer!
MAKE:=$(MAKE) --no-builtin-rules
endif

ifndef OUTDIR_NAME
OUTDIR_NAME=out
endif

# directory names:
buildprefix=$(depth)
outdir=$(OUTDIR_NAME)

# derived names
lily_bindir = $(depth)/bin
distdir = $(depth)/$(outdir)/$(DIST_NAME)
module-distdir = $(depth)/$(MODULE_DIST_NAME)
depdir = $(outdir)

flowerout = $(buildprefix)/flower/$(OUTDIR_NAME)
libout = $(buildprefix)/lib/$(OUTDIR_NAME)
lilyout = $(buildprefix)/lily/$(OUTDIR_NAME)
mi2muout = $(buildprefix)/mi2mu/$(OUTDIR_NAME)
makeout = $(buildprefix)/make/$(OUTDIR_NAME)
docout = $(buildprefix)/Documentation/$(OUTDIR_NAME)
binout = $(buildprefix)/bin/$(OUTDIR_NAME)

doc-dir = $(depth)/Documentation
flower-dir = $(depth)/flower
lib-dir = $(depth)/lib
lily-dir = $(depth)/lily
mi2mu-dir = $(depth)/mi2mu
make-dir = $(depth)/make
include-lib = $(depth)/lib/include
include-flower = $(depth)/flower/include


rpm-sources = ${HOME}/rpms/SOURCES
#

include $(makeout)/Configure_variables.make

# user settings:
#
include $(depth)/make/User.make
#
#
# need to be defined in local Makefiles:
# build = ./$(depth)/lily/$(outdir)/.build ######## UGR!
BUILD = $(shell cat $(build))
INCREASE_BUILD = echo `expr \`cat $(build)\` + 1` > .b; mv .b $(build)
#
FOOBAR = 
# the version:
#
ifneq ($(MY_PATCH_LEVEL),$(FOOBAR))
VERSION=$(MAJOR_VERSION).$(MINOR_VERSION).$(PATCH_LEVEL).$(MY_PATCH_LEVEL)
else
VERSION=$(MAJOR_VERSION).$(MINOR_VERSION).$(PATCH_LEVEL)
endif

ifneq ($(TOPLEVEL_MY_PATCH_LEVEL),$(FOOBAR))
TOPLEVEL_VERSION=$(TOPLEVEL_MAJOR_VERSION).$(TOPLEVEL_MINOR_VERSION).$(TOPLEVEL_PATCH_LEVEL).$(TOPLEVEL_MY_PATCH_LEVEL)
else
TOPLEVEL_VERSION=$(TOPLEVEL_MAJOR_VERSION).$(TOPLEVEL_MINOR_VERSION).$(TOPLEVEL_PATCH_LEVEL)
endif

#


# module and top level dist:
#
# fix naming, use TOPLEVEL_ prefix _or_ MODULE?
MODULE_DIST_NAME = $(MODULE_NAME)-$(VERSION)
DIST_NAME = lilypond-$(TOPLEVEL_VERSION)
NO_DOOS_DIST = bin flower lib lily make mi2mu out
#

# list of object files:
#
SOURCE_FILES = $(CCFILES) $(EXTRA_SOURCE_FILES)
OFILEC = $(SOURCE_FILES:.c=.o)
OFILECC = $(OFILEC:.cc=.o)
OFILEL = $(OFILECC:.l=.o)
OFILEY = $(OFILEL:.y=.o)
OFILES = $(addprefix $(outdir)/,$(OFILEY))
#

# dummydeps
#
DUMMYDEPS=\
 $(flowerout)/dummy.dep\
 $(libout)/dummy.dep\
 $(lilyout)/dummy.dep\
 $(mi2muout)/dummy.dep\

#

# clean file lists:
#
ERROR_LOG = 2> /dev/null
SILENT_LOG = 2>&1 >  /dev/null
date = $(shell date +%x)

# version stuff:
#
lily-version = $(lilyout)/version.hh
flower-version = $(flowerout)/version.hh
mi2mu-version = $(mi2muout)/version.hh
#

# custom libraries:
#
LIBFLOWER = $(flowerout)/$(LIB_PREFIX)flower$(LIB_SUFFIX)
LIBLILY = $(libout)/$(LIB_PREFIX)lily$(LIB_SUFFIX)
#

# compile and link options:
#
ARFLAGS = ru
CFLAGS = $(ICFLAGS) $(DEFINES) $(INCLUDES) $(USER_CFLAGS) $(EXTRA_CFLAGS)

# added two warnings that are treated by cygwin32's gcc 2.7.2 as errors.
# huh, but still, no warnings even provoced with linux's gcc 2.7.2.1?

# -pipe makes it go faster, but is not supported on all platforms. 
# EXTRA_CXXFLAGS= -fno-rtti -fno-exceptions -Wall -W -Wmissing-prototypes -Wmissing-declarations -Wconversion
EXTRA_CXXFLAGS= -Wall -W -Wmissing-prototypes -Wmissing-declarations -Wconversion

CXXFLAGS = $(CFLAGS) $(USER_CXXFLAGS) $(EXTRA_CXXFLAGS) $(MODULE_CXXFLAGS)
INCLUDES = -Iinclude -I$(outdir) -I$(include-lib) -I$(libout) -I$(include-flower) -I$(flowerout) 
CXX_OUTPUT_OPTION = $< -o $@
LDFLAGS = $(ILDFLAGS) $(USER_LDFLAGS) $(EXTRA_LDFLAGS) $(MODULE_LDFLAGS) -L$(depth)/lib/$(OUTDIR_NAME) -L$(depth)/flower/$(OUTDIR_NAME)
LOADLIBES = $(EXTRA_LIBES) $(MODULE_LIBES) -lg++ # need lg++ for win32, really!
#

# macro compiler:
#
M4 = m4
# 

# librarian:
#
AR = ar
AR_COMMAND = $(AR) $(ARFLAGS) $@
#
RANLIB_COMMAND=$(RANLIB) $@
# compiler:
#

DO_CXX_COMPILE=$(DODEP)\
	$(CXX) -c $(CXXFLAGS) $(CXX_OUTPUT_OPTION) 

# linker:
#
LD = $(CXX)
LD_COMMAND = $(LD) $(LDFLAGS) -o $@
#

# dependencies:
#
depfile = $(depdir)/$(subst .o,.dep,$(notdir $@)) 
DODEP=rm -f $(depfile); DEPENDENCIES_OUTPUT="$(depfile) $(outdir)/$(notdir $@)"
#

# utils:
#
#FLEX = flex
#BISON = bison
#

# generic target names:
#
ifdef NAME
EXECUTABLE = $(outdir)/$(NAME)$(EXE)
else
EXECUTABLE =
endif

EXECUTABLES = $(EXECUTABLE)
LIB_PREFIX = lib

ifndef LIB_SUFFIX
LIB_SUFFIX = .a
endif

LIBRARY = $(LIB_PREFIX)$(NAME)$(LIB_SUFFIX)
#

#replace to do stripping of certain objects
STRIPDEBUG=true 

DISTFILES=$(EXTRA_DISTFILES) Makefile $(ALL_SOURCES)
DOCDIR=$(depth)/$(outdir)

pod2html=pod2html
pod2groff=pod2man --center="LilyPond documentation" --section="0"\
	--release="LilyPond $(TOPLEVEL_MAJOR_VERSION).$(TOPLEVEL_MINOR_VERSION).$(TOPLEVEL_PATCH_LEVEL)" $< > $@


STRIP=strip --strip-debug
ifdef stablecc
 STABLEOBS=$(addprefix $(outdir)/,$(stablecc:.cc=.o))
endif

# substitute $(STRIP) in Site.make if you want stripping
DO_STRIP=true


