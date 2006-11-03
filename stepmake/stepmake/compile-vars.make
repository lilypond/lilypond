ARFLAGS = ru

ALL_LDFLAGS = $(LDFLAGS) $(CONFIG_LDFLAGS) $($(PACKAGE)_LDFLAGS) $(MODULE_LDFLAGS) $(CONFIG_LDFLAGS)

ifeq ($(MINGW_BUILD),)
PIC_FLAGS = -fpic -fPIC
SHARED_FLAGS = -shared
else
SHARED_FLAGS = -mdll
endif

o-dep-out = $(outdir)/$(subst .o,.dep,$(notdir $@))#
DO_O_DEP = rm -f $(o-dep-out); DEPENDENCIES_OUTPUT="$(o-dep-out) $(outdir)/$(notdir $@)"

lo-dep-out = $(outdir)/$(subst .lo,.dep,$(notdir $@))#
DO_LO_DEP = rm -f $(lo-dep-out); DEPENDENCIES_OUTPUT="$(lo-dep-out) $(outdir)/$(notdir $@)"

