top-build-dir = $(shell cd $(depth) && pwd)
build-dir = $(shell cd . && pwd)
tree-dir = $(subst $(top-build-dir),,$(build-dir))

absdir = $(shell cd $(1) ; pwd)


ifneq ($(configure-srcdir),.)
srcdir-build = 1
endif

ifndef srcdir-build
src-depth = $(depth)
else
src-depth = $(configure-srcdir)
endif

top-src-dir := $(shell cd $(src-depth); pwd)

ifndef srcdir-build
src-dir = .
else
src-dir = $(top-src-dir)$(tree-dir)
VPATH = $(src-dir)
endif

abs-src-dir = $(top-src-dir)$(tree-dir)

.UNEXPORT: build-dir src-dir tree-dir

src-wildcard = $(subst $(src-dir)/,,$(wildcard $(src-dir)/$(1)))

ifeq ($(distdir),)
  distdir = $(top-build-dir)/$(outdir)/$(DIST_NAME)
  DIST_NAME = $(package)-$(TOPLEVEL_VERSION)
endif
distname = $(package)-$(TOPLEVEL_VERSION)

doc-dir = $(src-depth)/Documentation
po-dir = $(src-depth)/po
step-bindir = $(stepmake)/bin

# stepmake package support.
DEPTH = $(depth)/$(package-depth)

INSTALLPY=$(PYTHON) $(step-bindir)/install.py -c
INSTALL=$(INSTALLPY)

group-dir = $(shell cd $(DEPTH);pwd)/..
patch-dir = $(group-dir)/patches
rpm-sources = $(release-dir)
rpm-build = $(group-dir)/RedHat/BUILD
package-icon = $(outdir)/$(package)-icon.xpm

ifneq ($(strip $(MY_PATCH_LEVEL)),)
VERSION=$(MAJOR_VERSION).$(MINOR_VERSION).$(PATCH_LEVEL).$(MY_PATCH_LEVEL)
else
VERSION=$(MAJOR_VERSION).$(MINOR_VERSION).$(PATCH_LEVEL)
endif

ifneq ($(strip $(TOPLEVEL_MY_PATCH_LEVEL)),)
TOPLEVEL_VERSION=$(TOPLEVEL_MAJOR_VERSION).$(TOPLEVEL_MINOR_VERSION).$(TOPLEVEL_PATCH_LEVEL).$(TOPLEVEL_MY_PATCH_LEVEL)
else
TOPLEVEL_VERSION=$(TOPLEVEL_MAJOR_VERSION).$(TOPLEVEL_MINOR_VERSION).$(TOPLEVEL_PATCH_LEVEL)
endif


# no local settings in the build process.
LANG=
export LANG


INFO_DIRECTORIES = Documentation/user input/lsr

# clean file lists:
#
ERROR_LOG = 2> /dev/null
SILENT_LOG = 2>&1 >  /dev/null
date := $(shell date +%x)	#duplicated?

INCLUDES = $(src-dir)/include $(outdir) $($(PACKAGE)_INCLUDES) $(MODULE_INCLUDES)

M4 = m4

DIST_FILES=$(EXTRA_DIST_FILES) GNUmakefile $(ALL_SOURCES) $(call src-wildcard,SConscript)
DOCDIR=$(depth)/$(outdir)

#?
STRIPDEBUG=true
STRIP=strip --strip-debug
DO_STRIP=true

LOOP=+$(foreach i, $(SUBDIRS), $(MAKE) PACKAGE=$(PACKAGE) package=$(package) -C $(i) $@ &&) true

ETAGS_FLAGS =
CTAGS_FLAGS =

makeflags=$(patsubst %==, %, $(patsubst ---%,,$(patsubst ----%,,$(MAKEFLAGS:%=--%))))

IN_FILES := $(call src-wildcard,*.in)
SOURCE_FILES += $(IN_FILES)

# Preprocessed .in documentation _FILES:
OUTIN_FILES = $(addprefix $(outdir)/, $(IN_FILES:%.in=%))

ALL_SOURCES = $(SOURCE_FILES)

ifeq (cygwin,$(findstring cygwin,$(HOST_ARCH)))
CYGWIN_BUILD = yes
endif

ifeq (mingw,$(findstring mingw,$(HOST_ARCH)))
MINGW_BUILD = yes
endif

ifeq (darwin,$(findstring darwin,$(HOST_ARCH)))
DARWIN_BUILD = yes
endif
