
export PATH:=$(topdir)/lily/out:$(topdir)/buildscripts/out:$(PATH)
export MFINPUTS:=$(topdir)/mf/$(PATHSEP)$(MFINPUTS)$(PATHSEP)$(PATHSEP)
export TEXINPUTS:=$(topdir)/tex/$(PATHSEP)$(topdir)/ps/$(PATHSEP)$(TEXINPUTS)$(PATHSEP)..$(PATHSEP)$(PATHSEP)
export LILYINCLUDE:=$(topdir)/scm$(PATHSEP)$(topdir)/ly$(PATHSEP)$(topdir)/mf/out$(PATHSEP)$(PATHSEP)$(topdir)/tfm:$(LILYINCLUDE)

