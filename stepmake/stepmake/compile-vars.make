ARFLAGS = ru

LDFLAGS = $(ILDFLAGS) $(EXTRA_LDFLAGS) $($(PACKAGE)_LDFLAGS) $(MODULE_LDFLAGS) $(USER_LDFLAGS)

PIC_FLAGS = -fpic -fPIC
SHARED_FLAGS = -shared

o-dep-out = $(outdir)/$(subst .o,.dep,$(notdir $@))#
DO_O_DEP = rm -f $(o-dep-out); DEPENDENCIES_OUTPUT="$(o-dep-out) $(outdir)/$(notdir $@)"

lo-dep-out = $(outdir)/$(subst .lo,.dep,$(notdir $@))#
DO_LO_DEP = rm -f $(lo-dep-out); DEPENDENCIES_OUTPUT="$(lo-dep-out) $(outdir)/$(notdir $@)"

