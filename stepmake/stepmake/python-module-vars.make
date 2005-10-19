PY_MODULES_IN = $(call src-wildcard,*.py)
OUT_PY_MODULES = $(PY_MODULES_IN:%=$(outdir)/%)
OUT_PYC_MODULES = $(OUT_PY_MODULES:%.py=%.pyc)
ifneq ($(CYGWIN_BUILD),)
SHARED_MODULE_SUFFIX = .dll
else
SHARED_MODULE_SUFFIX = .so
endif
OUT_SO_MODULES = $(addprefix $(outdir)/, $(C_FILES:.c=$(SHARED_MODULE_SUFFIX)))
EXTRA_DIST_FILES += $(PY_MODULES_IN)

