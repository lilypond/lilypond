# lib/GNUmakefile
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

#ugh
biltdir=../../build/lib

make-in-build: wild-check
	exec $(MAKE) -C $(biltdir)

# include Makefile
include $(biltdir)/Makefile

clean: wild-check
	exec $(MAKE) -C $(biltdir) $@

dist: wild-check
	exec $(MAKE) -C $(biltdir) $@

distclean: wild-check
	exec $(MAKE) -C $(biltdir) $@

endif

# Makefile.am.wild: liblily_la_SOURCES = $(wildcard *.cc)
CURRENT_liblily_la_SOURCES = $(wildcard *.cc)

wild-check: check-lily-la-sources

check-lily-la-sources:
ifneq ($(liblily_la_SOURCES),$(CURRENT_libflower_la_SOURCES))
	@echo \"$(liblily_la_SOURCES)\"
	@echo \"$(CURRENT_liblily_la_SOURCES)\"
	@echo  liblily_la_SOURCES changed: rerunning make-wild.
	@-rm -f Makefile.am
	$(PERL) $(top_srcdir)/bin/wild-perl < Makefile.am.wild > Makefile.am
endif

