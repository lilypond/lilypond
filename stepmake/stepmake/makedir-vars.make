
MAKE_FILES := $(call src-wildcard,*.make)
LSM_FILES:= $(call src-wildcard,*.lsm.in)
SPEC_FILES:= $(call src-wildcard,*.spec.in)

OUTLSM_FILES=$(addprefix $(outdir)/,$(basename $(LSM_FILES)))
OUTSPEC_FILES=$(addprefix $(outdir)/,$(basename $(SPEC_FILES)))
OUT_DIST_FILES= $(strip $(OUTLSM_FILES) $(OUTSPEC_FILES))

EXTRA_DIST_FILES += $(MAKE_FILES)

# these two outdir FILES are distributed, since they make sense to have
# without running configure and make.

at-dir = $(doc-dir)/
at-ext = .in


