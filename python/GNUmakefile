depth = ..

SUBDIRS=auxiliar

STEPMAKE_TEMPLATES=install-out install po

PY_MODULES_IN = $(filter-out %_test.py, $(call src-wildcard,*.py))

include $(depth)/make/stepmake.make

$(outdir)/%.pyc.dummy: %.py
	$(call ly_progress,Making,$@,(py compile))
	# Do not use buildscript-dir, this has not been traversed yet.
	$(PYTHON) $(top-src-dir)/scripts/build/compile.py $< $(outdir)
	touch $@

default: $(PY_MODULES_IN:%.py=$(outdir)/%.pyc.dummy)

INSTALLATION_DIR = $(local_lilypond_datadir)/python
INSTALLATION_FILES = $(PY_MODULES_IN)

INSTALLATION_OUT_DIR = $(local_lilypond_datadir)/python/__pycache__
INSTALLATION_OUT_FILES = $(wildcard $(outdir)/__pycache__/*.pyc)

local-test: book_base_test.py
	$(PYTHON) $<
