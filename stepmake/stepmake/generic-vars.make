# title	   generic variables
# file	   make/Variables.make
#
# do not change this file for site-wide extensions; please use 
# make/$(outdir)/Site.make; 
#
# Any change in files in this directory (make/) would be distributed, if 
# you do make dist 

# directory names:

# depth from group-dir
# not eh, normally used
DEPTH = $(depth)/$(package-depth)

topdir := $(shell cd $(depth); pwd)
pwd := $(shell pwd)


# derived names
ifeq ($(distdir),)
  distdir = $(depth)/$(outdir)/$(DIST_NAME)
  DIST_NAME = $(package)-$(TOPLEVEL_VERSION)
endif
distname = $(package)-$(TOPLEVEL_VERSION)




# obsolete?
makeout = $(depth)/make/$(outdir)
docout = $(depth)/Documentation/$(outdir)
binout = $(depth)/bin/$(outdir)

doc-dir = $(depth)/Documentation
po-dir = $(depth)/po

# sort-out which of these are still needed
#
$(package)_bindir = $(depth)/bin
step-bindir = $(depth)/$(stepmake)/bin
abs-step-bindir = $(topdir)/$(stepmake)/bin
#
group-dir = $(shell cd $(DEPTH)/..; pwd)
release-dir = $(group-dir)/releases
patch-dir = $(group-dir)/patches
#
# i have in $HOME/.rpmrc
#     topdir: /home/fred/usr/src/Redhat
#
rpm-sources = $(release-dir)
rpm-build = $(group-dir)/RedHat/BUILD
#


# need to be defined in local Makefiles:
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



# clean file lists:
#
ERROR_LOG = 2> /dev/null
SILENT_LOG = 2>&1 >  /dev/null
date := $(shell date +%x)	#duplicated?

# compile and link options:
#
ARFLAGS = ru

INCLUDES =  include $(outdir) $($(PACKAGE)_INCLUDES)
LDFLAGS = $(ILDFLAGS) $(USER_LDFLAGS) $(EXTRA_LDFLAGS) $(MODULE_LDFLAGS) $($(PACKAGE)_LDFLAGS)

MODULE_LIBES=$(addsuffix /$(outdir)/library.a, $(MODULE_LIBS))
LOADLIBES = $(MODULE_LIBES) $($(PACKAGE)_LIBES) $(EXTRA_LIBES)
# urg: for windows ?
# LOADLIBES = $(MODULE_LIBES) $($(PACKAGE)_LIBES) $(EXTRA_LIBES) -lstdc++
#

# macro compiler:
#
M4 = m4
# 

#
LD_COMMAND = $(LD) $(LDFLAGS) -o $@
#

# dependencies:
#
depfile = $(outdir)/$(subst .o,.dep,$(notdir $@))#
DODEP=rm -f $(depfile); DEPENDENCIES_OUTPUT="$(depfile) $(outdir)/$(notdir $@)"
#


# generic target names:
#
ifdef NAME
EXECUTABLE = $(outdir)/$(NAME)$(EXE)
else
EXECUTABLE =
endif

EXECUTABLES = $(notdir $(EXECUTABLE))


#

#replace to do stripping of certain objects
STRIPDEBUG=true 

DIST_FILES=$(EXTRA_DIST_FILES) GNUmakefile $(ALL_SOURCES)
DOCDIR=$(depth)/$(outdir)


STRIP=strip --strip-debug
ifdef stablecc
 STABLEOBS=$(addprefix $(outdir)/,$(stablecc:.cc=.o))
endif

# substitute $(STRIP) in Site.make if you want stripping
DO_STRIP=true
LOOP=$(foreach i,  $(SUBDIRS),  $(MAKE) -C $(i) $@ &&) true


include $(stepdir)/files.make
