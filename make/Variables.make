#
# project  LilyPond -- the musical typesetter
# title	   generic variables
# file	   make/Variables.make
# abstract do not change this file for site-wide extensions;
# please edit settings in User.make 
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>

# toplevel version info, might be handy?
#
include ./$(depth)/.version
#
include ./$(depth)/make/out/Configure_variables.make

ifeq (0,${MAKELEVEL})
MAKE:=$(MAKE) --no-builtin-rules
endif


# directory names:
#
outdir = out# "objects" won-t do, used for libs and deps as well
lily_bindir = ./$(depth)/bin
distdir = ./$(depth)/$(DIST_NAME)
module-distdir = ./$(depth)/$(MODULE_DIST_NAME)
depdir = $(outdir)
flowerout = ./$(depth)/flower/$(outdir)
libout = ./$(depth)/lib/$(outdir)
libdir = $(outdir)
lilyout = ./$(depth)/lily/$(outdir)
mi2muout = ./$(depth)/mi2mu/$(outdir)
makeout = ./$(depth)/make/$(outdir)
flower-dir = ./$(depth)/flower
lib-dir = ./$(depth)/lib
lily-dir = ./$(depth)/lily
mi2mu-dir = ./$(depth)/mi2mu
make-dir = ./$(depth)/make
include-lib = ./$(depth)/lib/include
include-flower = ./$(depth)/flower/include
#

# user settings:
#
include ./$(depth)/make/User.make
#
#
# need to be defined in local Makefiles:
# build = ./$(depth)/lily/$(outdir)/.build ######## UGR!
BUILD = $(shell cat $(build))
INCREASE_BUILD = echo `expr \`cat $(build)\` + 1` > .b; mv .b $(build)
#

# ugh, for win32 make
export CXX

# the version:
#
VERSION=$(MAJOR_VERSION).$(MINOR_VERSION).$(PATCH_LEVEL)$(MY_PATCH_LEVEL)
TOPLEVEL_VERSION=$(TOPLEVEL_MAJOR_VERSION).$(TOPLEVEL_MINOR_VERSION).$(TOPLEVEL_PATCH_LEVEL)$(TOPLEVEL_MY_PATCH_LEVEL)
#


# module and top level dist:
#
# fix naming, use TOPLEVEL_ prefix _or_ MODULE?
MODULE_DIST_NAME = $(MODULE_NAME)-$(VERSION)
DIST_NAME = lilypond-$(TOPLEVEL_VERSION)
#

# list of object files:
#
SOURCE_FILES = $(CCFILES) $(EXTRA_SOURCE_FILES)
OFILEC = $(SOURCE_FILES:.c=.o)
OFILECC = $(OFILEC:.cc=.o)
OFILEL = $(OFILECC:.l=.o)
OFILEY = $(OFILEL:.y=.o)
OFILES = $(patsubst %,$(outdir)/%,$(OFILEY))
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
SILENT_LOG = >& /dev/null
allexe = $(lily_bindir)/lilypond $(lily_bindir)/mi2mu
allhh := $(shell $(FIND) -name "*.hh" $(ERROR_LOG))
allcc := $(shell $(FIND) -name "*.cc" $(ERROR_LOG))
allobs := $(shell $(FIND) $(outdir) -name "*.o" $(ERROR_LOG))
allibs := $(shell $(FIND) $(libdir) -name "*.lib" $(ERROR_LOG))
alldeps := $(shell $(FIND) $(outdir) -name "*.dep" $(ERROR_LOG))

# version stuff:
#
lily-version = $(lilyout)/version.hh
flower-version = $(flowerout)/version.hh
mi2mu-version = $(mi2muout)/version.hh
#

# custom libraries:
#
LIBFLOWER = $(depth)/flower/$(outdir)/$(LIB_PREFIX)flower$(LIB_SUFFIX)
LIBLILY = $(depth)/lib/$(outdir)/$(LIB_PREFIX)lily$(LIB_SUFFIX)
#

# compile and link options:
#
ARFLAGS = ru
CFLAGS = $(DEFINES) $(INCLUDES) $(USER_CFLAGS) $(EXTRA_CFLAGS)
CXXFLAGS = $(CFLAGS) $(USER_CXXFLAGS) $(EXTRA_CXXFLAGS)
INCLUDES = -Iinclude -I$(outdir) -I$(include-lib) -I$(libout) -I$(include-flower) -I$(flowerout) 
CXX_OUTPUT_OPTION = $< -o $@
LDFLAGS = $(EXTRA_LDFLAGS)
LOADLIBES = $(EXTRA_LIBES) $(CUSTOMLIBES)
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
# "CC = $(CC)"
# "CXX = $(CXX)"
#

# linker:
#
LD = $(CXX)
LD_COMMAND = $(LD) $(LDFLAGS) -o $@
#

# dependencies:
#
depfile = ./$(depdir)/$(subst .o,.dep,$(notdir $@)) 
DODEP=rm -f $(depfile); DEPENDENCIES_OUTPUT="$(depfile) $(outdir)/$(notdir $@)"
#

# utils:
#
#FLEX = flex
#BISON = bison
#

# generic target names:
#
EXECUTABLE = $(NAME)$(EXE)
LIB_PREFIX = lib
LIB_SUFFIX = .a
LIBRARY = $(LIB_PREFIX)$(NAME)$(LIB_SUFFIX)
#

STRIPDEBUG=true #replace to do stripping of certain objects

DISTFILES=$(EXTRA_DISTFILES) Makefile $(ALL_SOURCES)
DOCDIR=$(depth)/$(outdir)


progdocs=$(allhh) $(allcc) 
pod2groff=pod2man --center="LilyPond documentation" --section="0"\
	--release="LilyPond $(TOPLEVEL_MAJOR_VERSION).$(TOPLEVEL_MINOR_VERSION).$(TOPLEVEL_PATCH_LEVEL)" $< > $@
