depth = ..

SUBDIRS=auxiliar

STEPMAKE_TEMPLATES=python-module install-out po

include $(depth)/make/stepmake.make

# unset al guile stuff from configure
CONFIG_LDFLAGS=

INSTALLATION_OUT_DIR=$(local_lilypond_datadir)/python
INSTALLATION_OUT_FILES=$(OUT_PY_MODULES)

default: $(outdir)/relocate-preamble.py

$(outdir)/relocate-preamble.py: $(depth)/VERSION

local-test: book_base_test.py
	$(PYTHON) $<
