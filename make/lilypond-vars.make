
export PATH:=$(topdir)/lily/out:$(topdir)/buildscripts/out:$(PATH)
export MFINPUTS:=$(topdir)/mf/$(PATHSEP)$(MFINPUTS)$(PATHSEP)$(PATHSEP)
export TEXINPUTS:=$(topdir)/tex/$(PATHSEP)$(TEXINPUTS)$(PATHSEP)$(PATHSEP)
export LILYINCLUDE:=$(topdir)/init$(PATHSEP)$(topdir)/mf/out$(PATHSEP)$(LILYINCLUDE)

