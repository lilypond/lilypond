
export PATH:=$(topdir)/lily/out:$(topdir)/buildscripts/out:$(PATH)

# Huh, PATHSEP, but still '/' for dirsep?
# Doesn't make sense.
ifeq (0,1)

export MFINPUTS:=$(topdir)/mf/$(PATHSEP)$(MFINPUTS)$(PATHSEP)$(PATHSEP)
export TEXINPUTS:=$(topdir)/mf/out/$(PATHSEP)$(topdir)/tex/$(PATHSEP)$(topdir)/ps/$(PATHSEP)$(TEXINPUTS)$(PATHSEP)$(pwd)$(PATHSEP)$(PATHSEP)
export LILYINCLUDE:=$(topdir)/ps$(PATHSEP)$(topdir)/scm$(PATHSEP)$(topdir)/ly$(PATHSEP)$(topdir)/mf/out$(PATHSEP)$(PATHSEP)$(TEX_TFMDIR)$(PATHSEP)$(LILYINCLUDE)

else

export MFINPUTS:=$(topdir)/mf/:$(MFINPUTS)::
export TEXINPUTS:=$(topdir)/mf/out/:$(topdir)/tex/:$(topdir)/ps/:$(TEXINPUTS):$(pwd)::
export LILYINCLUDE:=$(topdir)/ps:$(topdir)/scm:$(topdir)/ly:$(topdir)/mf/out::$(TEX_TFMDIR):$(LILYINCLUDE)

endif

export LILYPONDPREFIX:=$(depth)/
