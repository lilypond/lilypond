# GNUmakefile

#ugh
Makefile=$(wildcard Makefile)
# are we configured here, or are we using --srcdir
ifeq ($(Makefile),Makefile)

include Makefile

biltdir=.

else

#ugh
#ugh
biltdir=../build

make-in-build: wild-check
	exec $(MAKE) -C $(biltdir)

# include Makefile
include $(biltdir)/Makefile

SUBDIRS :=

clean: wild-check
	exec $(MAKE) -C $(biltdir) $@

dist: wild-check
	exec $(MAKE) -C $(biltdir) $@

distclean: wild-check
	exec $(MAKE) -C $(biltdir) $@

endif

wild-check:
