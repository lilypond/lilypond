# test/GNUmakefile
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
biltdir=../flowertest

make-in-build: wild-check
	exec $(MAKE) -C $(biltdir)

# include Makefile
include $(biltdir)/Makefile

endif

# Makefile.am.wild: TEST_SCRIPTS = $(wildcard *.sh)
CURRENT_TEST_SCRIPTS = $(wildcard *.sh)

wild-check: check-test-scripts

check-test-scripts:
ifneq ($(TEST_SCRIPTS),$(CURRENT_TEST_SCRIPTS))
	@echo \"$(TEST_SCRIPTS)\"
	@echo \"$(CURRENT_TEST_SCRIPTS)\"
	@echo  TEST_SCRIPTS changed: rerunning make-wild.
	@-rm -f Makefile.am
	$(PERL) $(top_srcdir)/bin/wild-perl < Makefile.am.wild > Makefile.am
endif

