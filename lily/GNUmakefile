# lilypond/GNUmakefile
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
biltdir=../../build/lily

make-in-build: wild-check
	echo exec $(MAKE) -C $(biltdir)
	exec $(MAKE) -C $(biltdir)

include $(biltdir)/Makefile

clean: wild-check
	exec $(MAKE) -C $(biltdir) $@

dist: wild-check
	exec $(MAKE) -C $(biltdir) $@

distclean: wild-check
	exec $(MAKE) -C $(biltdir) $@

lexer.cc: wild-check
	exec $(MAKE) -C $(biltdir) $@

parser.hh: wild-check
	exec $(MAKE) -C $(biltdir) $@

parser.cc: wild-check
	exec $(MAKE) -C $(biltdir) $@

#ugh OMIT_DEPENDENCIES does not seem to work
.deps/lexer.P:
	exec $(MAKE) -C $(biltdir) $@

.deps/parser.P:
	exec $(MAKE) -C $(biltdir) $@

endif

# Makefile.am.wild: lilypond_SOURCES = $(wildcard *.cc *.ll *.yy)
# ugh
CURRENT_lilypond_SOURCES = $(wildcard *.cc lexer.ll parser.yy)

# ugh: stupid lexer.cc/parser.cc cluttering sourcedir!
wild-check: check-lilypond-sources

check-lilypond-sources:
ifneq ($(lilypond_SOURCES),$(filter-out $(IGNORE_WILDCARDS), $(CURRENT_lilypond_SOURCES)))
	@echo \"$(lilypond_SOURCES)\"
	@echo \"$(CURRENT_lilypond_SOURCES)\"
	@echo  lilypond_SOURCES changed: rerunning make-wild.
	@-rm -f Makefile.am
	$(PERL) $(top_srcdir)/bin/wild-perl < Makefile.am.wild > Makefile.am
endif

