# directory names:

# depth from group-dir
# internal, not normally used
DEPTH = $(depth)/$(package-depth)

ifeq ($(topdir),)
topdir := $(shell cd $(depth); pwd)
endif
pwd := $(shell pwd)

# $(depth) is deprecated, for most cases you'll want $(src-depth)
#
# Well, on second thought.
# It can do no harm, but using src-depth iso depth is only necessary
# for broken rules that do
#    cd $(outdir) && foo  $(depth) ...
src-depth = $(depth)/$(srcdir)

# derived names
ifeq ($(distdir),)
  distdir = $(topdir)/$(outdir)/$(DIST_NAME)
  DIST_NAME = $(package)-$(TOPLEVEL_VERSION)
endif
distname = $(package)-$(TOPLEVEL_VERSION)

# obsolete?
#makeout = $(depth)/make/$(outdir)
#docout = $(depth)/Documentation/$(outdir)
#binout = $(depth)/bin/$(outdir)

doc-dir = $(src-depth)/Documentation
po-dir = $(src-depth)/po

# sort-out which of these are still needed
#
$(package)_bindir = $(depth)/bin
step-bindir = $(stepmake)/bin

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

# package-icon=$(outdir)/$(package)-icon.gif
package-icon=$(outdir)/$(package)-icon.xpm


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

#INCLUDES =  $(depth)/$(builddir) include $(outdir) $($(PACKAGE)_INCLUDES) $(MODULE_INCLUDES)
INCLUDES = include $(outdir) $($(PACKAGE)_INCLUDES) $(MODULE_INCLUDES)

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
LOOP=$(foreach i,  $(SUBDIRS), $(MAKE) PACKAGE=$(PACKAGE) package=$(package) -C $(i) $@ &&) true

# different redhat releases need different flags for etags. Just use defaults.
ETAGS_FLAGS= # -CT
CTAGS_FLAGS=-h

makeflags=$(patsubst %==, %, $(patsubst ---%,,$(patsubst ----%,,$(MAKEFLAGS:%=--%))))

DEP_FILES := $(wildcard $(outdir)/*.dep)

IN_FILES := $(wildcard *.in)
SOURCE_FILES += $(IN_FILES)

# Preprocessed .in documentation _FILES:
OUTIN_FILES = $(addprefix $(outdir)/, $(IN_FILES:%.in=%))

ALL_SOURCES = $(SOURCE_FILES)


