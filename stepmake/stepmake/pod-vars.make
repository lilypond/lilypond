POD_FILES := $(call src-wildcard,*.pod)

OUTPOD_FILES = $(addprefix $(outdir)/,$(POD_FILES))
