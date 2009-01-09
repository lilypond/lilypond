depth = ..

SUBDIRS=auxiliar build

SEXECUTABLES=convert-ly lilypond-book abc2ly etf2ly midi2ly lilypond-invoke-editor musicxml2ly lilysong lilymidi

STEPMAKE_TEMPLATES=script help2man po
LOCALSTEPMAKE_TEMPLATES = lilypond
HELP2MAN_EXECS = $(SEXECUTABLES)

AT_FILES=relocate-preamble
at-ext=.py
at-dir=$(top-build-dir)/python/$(outdir)

include $(depth)/make/stepmake.make
include $(stepdir)/executable-targets.make
