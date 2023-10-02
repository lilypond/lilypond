depth = ..

SUBDIRS=build

SEXECUTABLES = \
  abc2ly \
  convert-ly \
  etf2ly \
  lilypond-book \
  lilypond-invoke-editor \
  lilymidi \
  midi2ly \
  musicxml2ly

TEMPLATES = help2man lilypond po script
HELP2MAN_EXECS = $(SEXECUTABLES)

AT_FILES=relocate-preamble
at-ext=.py
at-dir=$(outdir)

include $(depth)/make/lilypond.make

$(PYTHON_SCRIPTS): $(outdir)/relocate-preamble.py

local-install:
	-$(INSTALLPY) -d $(DESTDIR)$(bindir)
	$(INSTALLPY) -c -m 755 $(addprefix $(outdir)/, $(SEXECUTABLES)) $(DESTDIR)$(bindir)

local-uninstall:
	$(foreach a, $(SEXECUTABLES), rm -f $(DESTDIR)$(bindir)/$(a) &&) true
