# lilypond/GNUmakefile
#
# This file helps maintainers to keep their Makefile.am automatically
# up to date using GNU make features.
# If you don't have (or run) GNU make, Makefile.am will not be updated
# automatically when source files are added/removed.

# automake/wild-make should generate this file from Makefile.am.wild

include Makefile

# Makefile.am.wild: lilypond_SOURCES = $(wildcard *.cc)
CURRENT_lilypond_SOURCES = $(wildcard *.cc)

# ugh: there is a dummy file 'wild-check' to satisfy the wild-check target
# for non-GNU makes in Makefile(.test)
# but we'll ignore that
# .PHONY: wild-check

# ugh: stupid lexer.cc/parser.cc cluttering sourcedir!
# but it works fine in other dirs (see ../mi2mu)
# wild-check: check-lilypond-sources

check-lilypond-sources:
ifneq ($(lilypond_SOURCES),$(CURRENT_mi2mu_SOURCES))
	@echo \"$(lilypond_SOURCES)\"
	@echo \"$(CURRENT_lilypond_SOURCES)\"
	@echo  lilypond_SOURCES changed: NOT yet rerunning make-wild.  Please type:
	@echo "$(PERL) $(top_srcdir)/bin/wild-perl < Makefile.am.wild > Makefile.am"
endif

