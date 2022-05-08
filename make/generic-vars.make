top-build-dir := $(realpath $(depth) )
build-dir := $(realpath  . )

tree-dir = $(build-dir:$(top-build-dir)%=%)

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

# Sort to make execution order deterministic. It also provides a
# poor man's progress indicator.
src-wildcard = $(sort $(subst $(src-dir)/,,$(wildcard $(src-dir)/$(1))))

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

INSTALLPY=$(PYTHON) $(buildscript-dir)/install.py $(INSTALLPY_QUIET) -c

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
LANG=C
export LANG


# texi2html iterates over section headers stored as entries of a map.
# Disable Perl's hash randomization to make the order reproducible.
PERL_HASH_SEED=0
export PERL_HASH_SEED


INFO_DIRECTORIES = Documentation

# clean file lists:
#
ERROR_LOG = 2> /dev/null
date := $(shell date +%x)	#duplicated?

INCLUDES = $(src-dir)/include $(outdir) $(MODULE_INCLUDES)

# Create a command to make targets in subdirectories.
# $(1) is a subset of $(SUBDIRS)
# $(2) is a list of targets
define make_subdirs
#       enforce order to avoid surprises due to implicit dependencies
	[ "$(1)" = "$(filter $(1),$(SUBDIRS))" ] || (echo "*** {$(1)} is not a subset of ordered set {$(SUBDIRS)}" 1>&2 && false)
	+$(foreach d, $(1), $(MAKE) -C $(d) $(2) &&) true
endef

LOOP=+$(foreach i, $(SUBDIRS), $(MAKE) -C $(i) $@ &&) true

ETAGS_FLAGS =
CTAGS_FLAGS =

# In verbose mode, command lines are shown before execution.
# It is thus not necessary to make `install.py` emit the same again.
ifdef VERBOSE
  INSTALLPY_QUIET = -q
endif

buildscript-dir = $(top-src-dir)/scripts/build
auxpython-dir = $(src-depth)/python/auxiliar
auxscript-dir = $(src-depth)/scripts/auxiliar
script-dir = $(src-depth)/scripts

export PYTHONPATH:=$(auxpython-dir):$(PYTHONPATH)

# texi2html v5 has fatal errors in the build, so only be strict about
# errors in the version we officially support
ifeq ($(TEXI2HTML_VERSION),1082000)
TEXI2HTML_ERROR_LIMIT=--error-limit=0
endif
