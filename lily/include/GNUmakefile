# lily/include/GNUmakefile
#
# This file helps maintainers to keep their Makefile.am automatically
# up to date using GNU make features.
# If you don't have (or run) GNU make, Makefile.am will not be updated
# automatically when source files are added/removed.

# automake/wild-make should generate this file from Makefile.am.wild

#ugh
Makefile=$(wildcard Makefile)
# are we configured here, or are we using --srcdir
ifeq ($(Makefile),Makefile)

include Makefile

biltdir=.

else

biltdir=../../../build/lily/include

# include Makefile
include $(biltdir)/Makefile

endif

# Makefile.am.wild: noinst_HEADERS = $(wildcard *.hh *.icc *.tcc)
CURRENT_noinst_HEADERS = $(wildcard *.hh *.icc *.tcc)

wild-check: check-noinst-headers


check-noinst-headers:
ifneq ($(noinst_HEADERS),$(CURRENT_noinst_HEADERS))
	@echo \"$(noinst_HEADERS)\"
	@echo \"$(CURRENT_noinst_HEADERS)\"
	@echo  noinst_HEADERS changed: rerunning make-wild.
	@-rm -f Makefile.am
	$(PERL) $(top_srcdir)/bin/wild-perl < Makefile.am.wild > Makefile.am
endif

