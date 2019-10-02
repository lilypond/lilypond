PY_MODULES_IN = $(call src-wildcard,*.py)
OUT_PY_MODULES = $(PY_MODULES_IN:%=$(outdir)/%)
