depth = ..

SUBDIRS=auxiliar

STEPMAKE_TEMPLATES=install-out po

PY_MODULES_IN = $(call src-wildcard,*.py)
OUT_PY_MODULES = $(PY_MODULES_IN:%=$(outdir)/%)

include $(depth)/make/stepmake.make

$(outdir)/%.py: %.py
	$(call ly_progress,Making,$@,(py compile))
	cp $< $@
	PYTHONOPTIMIZE= $(PYTHON) -c 'import py_compile; py_compile.compile ("$@", doraise=True)'
	chmod 755 $@

default: $(OUT_PY_MODULES)

INSTALLATION_OUT_SUFFIXES = 1 2

INSTALLATION_OUT_DIR1=$(local_lilypond_datadir)/python
INSTALLATION_OUT_FILES1=$(PY_MODULES_IN)

INSTALLATION_OUT_DIR2 = $(local_lilypond_datadir)/python/__pycache__
INSTALLATION_OUT_FILES2 = $(wildcard $(outdir)/__pycache__/*.pyc)

local-test: book_base_test.py
	$(PYTHON) $<
