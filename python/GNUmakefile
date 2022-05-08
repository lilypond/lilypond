depth = ..

SUBDIRS=auxiliar

TEMPLATES = install-out install po

PY_MODULES_IN = $(filter-out %_test.py, $(call src-wildcard,*.py))

include $(depth)/make/lilypond.make

$(outdir)/%.pyc.dummy: %.py $(buildscript-dir)/compile.py
	$(call ly_progress,Making,$@,(py compile))
	$(PYTHON) $(buildscript-dir)/compile.py $< $(outdir)
	touch $@

default: $(PY_MODULES_IN:%.py=$(outdir)/%.pyc.dummy)

INSTALLATION_DIR = $(local_lilypond_datadir)/python
INSTALLATION_FILES = $(PY_MODULES_IN)

INSTALLATION_OUT_DIR = $(local_lilypond_datadir)/python/__pycache__
INSTALLATION_OUT_FILES = $(wildcard $(outdir)/__pycache__/*.pyc)

local-test: book_base_test.py
	$(PYTHON) $<
