PY_MODULES = $(wildcard *.py)
PY_IN_FILES = $(wildcard *.py.in)
OUT_PY_MODULES = $($(addprefix $(outdir)/, $(PY_IN_FILES:.in=)
SHARED_LIB_SUFFIX = .so
OUT_SO_MODULES = $(addprefix $(outdir)/, $(C_FILES:.c=$(SHARED_LIB_SUFFIX)))

