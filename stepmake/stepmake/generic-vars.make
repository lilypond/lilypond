top-build-dir := $(realpath $(depth) )
build-dir := $(realpath  . )

tree-dir = $(subst $(top-build-dir),,$(build-dir))

ifneq ($(configure-srcdir),.)
srcdir-build = 1
endif

ifndef srcdir-build
src-depth = $(depth)
else
src-depth = $(configure-srcdir)
endif

top-src-dir := $(realpath $(src-depth))

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
po-srcdir = $(src-depth)/po
po-outdir = $(depth)/po/$(outdir)

# stepmake package support.
DEPTH = $(depth)/$(package-depth)

INSTALLPY=$(buildscript-dir)/install -c
INSTALL=$(INSTALLPY)

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


# no locale settings in the build process.
LANG=
export LANG


INFO_DIRECTORIES = Documentation

# clean file lists:
#
ERROR_LOG = 2> /dev/null
SILENT_LOG = 2>&1 >  /dev/null
date := $(shell date +%x)	#duplicated?

INCLUDES = $(src-dir)/include $(outdir) $($(PACKAGE)_INCLUDES) $(MODULE_INCLUDES)

M4 = m4

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
