
export PATH:=$(topdir)/lily/out:$(topdir)/buildscripts/out$(PATHSEP)$(PATH)
export MFINPUTS:=$(topdir)/mf/$(PATHSEP)$(PATHSEP)$(MFINPUTS)$(PATHSEP)
export TEXINPUTS:=$(topdir)/tex/$(PATHSEP)$(TEXINPUTS)$(PATHSEP)$(PATHSEP)
export LILYINCLUDE:=$(topdir)/init$(PATHSEP)$(topdir)/mf/out$(PATHSEP)$(LILYINCLUDE)

