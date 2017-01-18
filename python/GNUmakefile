depth = ..

SUBDIRS=auxiliar

STEPMAKE_TEMPLATES=python-module install-out po

include $(depth)/make/stepmake.make

CFLAGS += -DPy_BUILD_CORE -Wall $(PYTHON_CFLAGS)
LDFLAGS += $(PYTHON_LIBS)
# unset al guile stuff from configure
CONFIG_LDFLAGS=

INSTALLATION_OUT_SUFFIXES=1

INSTALLATION_OUT_FILES=$(OUT_SO_MODULES)
INSTALLATION_OUT_DIR=$(local_lilypond_libdir)/python

INSTALLATION_OUT_DIR1=$(local_lilypond_datadir)/python
INSTALLATION_OUT_FILES1=$(OUT_PY_MODULES) $(OUT_PYC_MODULES)

default: $(outdir)/relocate-preamble.py

$(outdir)/relocate-preamble.py: $(depth)/VERSION
