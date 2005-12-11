ALL_LDFLAGS = $(LDFLAGS)
PY_MODULES_IN = $(call src-wildcard,*.py)
OUT_PY_MODULES = $(PY_MODULES_IN:%=$(outdir)/%)
OUT_PYC_MODULES = $(OUT_PY_MODULES:%.py=%.pyc)
OUT_PYO_MODULES = $(OUT_PY_MODULES:%.py=%.pyo)
ifeq ($(MINGW_BUILD)$(CYGWIN_BUILD),)
SHARED_MODULE_SUFFIX = .so
else
ifneq ($(CYGWIN_BUILD),)
SHARED_MODULE_SUFFIX = .dll
endif
ifneq ($(MINGW_BUILD),)
SHARED_MODULE_SUFFIX = .dll
endif
endif
ifneq ($(DARWIN_BUILD),)
SHARED_FLAGS = -bundle -flat_namespace -framework Python
endif
OUT_SO_MODULES = $(addprefix $(outdir)/, $(C_FILES:.c=$(SHARED_MODULE_SUFFIX)))
EXTRA_DIST_FILES += $(PY_MODULES_IN)

