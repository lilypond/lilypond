PY_MODULES = $(wildcard *.py)
PY_IN_FILES = $(wildcard *.py.in)
OUT_PY_MODULES = $($(addprefix $(outdir)/, $(PY_IN_FILES:.in=)
ifneq ($(CYGWIN_BUILD),)
SHARED_MODULE_SUFFIX = .dll
else
SHARED_MODULE_SUFFIX = .so
endif
OUT_SO_MODULES = $(addprefix $(outdir)/, $(C_FILES:.c=$(SHARED_MODULE_SUFFIX)))

