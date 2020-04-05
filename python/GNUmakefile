depth = ..

SUBDIRS=auxiliar

STEPMAKE_TEMPLATES=install-out install po

PY_MODULES_IN = $(call src-wildcard,*.py)
OUT_PY_MODULES = $(PY_MODULES_IN:%=$(outdir)/%)

include $(depth)/make/stepmake.make

$(outdir)/%.py: %.py
	$(call ly_progress,Making,$@,(py compile))
	cp $< $@
	PYTHONOPTIMIZE= $(PYTHON) -c 'import py_compile; py_compile.compile ("$@", doraise=True)'
	chmod 755 $@

default: $(OUT_PY_MODULES)

INSTALLATION_DIR = $(local_lilypond_datadir)/python
INSTALLATION_FILES = $(filter-out $(call src-wildcard, *_test.py), $(PY_MODULES_IN))

INSTALLATION_OUT_DIR = $(local_lilypond_datadir)/python/__pycache__
INSTALLATION_OUT_FILES = $(wildcard $(outdir)/__pycache__/*.pyc)

local-test: book_base_test.py
	$(PYTHON) $<
