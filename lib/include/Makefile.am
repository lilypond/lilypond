# Generated automatically by wild-perl 0.1
# lib/include/Makefile

noinst_HEADERS = binary-source-file.hh duration-convert.hh duration.hh file-storage.hh input.hh mapped-file-storage.hh moment.hh plet.hh proto.hh simple-file-storage.hh source-file.hh source.hh warn.hh windhoos-suck-suck-suck-thank-you-cygnus.hh

EXTRA_DIST = Makefile.am.wild GNUmakefile

# override default target for all-am: to get an extra dependency
# all-am: Makefile $(PROGRAMS) wild-check
# ugh, automake doesn't generate the all-am target here...
all: Makefile $(HEADERS) wild-check

# dummy target for non-GNU makes; is overridden in GNUmakefile
wild-check:

$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@
