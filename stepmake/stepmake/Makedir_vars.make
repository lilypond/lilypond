

# stepmake/Makedir.shared.make
# urg: stuff shared between yodl/lily/stepmake

MAKE_FILES := $(wildcard *.make)
LSM_FILES:= $(wildcard *.lsm.in)
OUTLSM_FILES=$(addprefix $(outdir)/,$(basename $(LSM_FILES)))
SPEC_FILES:= $(wildcard *.spec.in)
OUTSPEC_FILES=$(addprefix $(outdir)/,$(basename $(SPEC_FILES)))
OUT_DIST_FILES= $(strip $(OUTLSM_FILES) $(OUTSPEC_FILES))

EXTRA_DIST_FILES += $(state-vector) $(MAKE_FILES)

# these two outdir FILES are distributed, since they make sense to have 
# without running configure and make.

AT_FILES = $(BLURBS) # 
at-dir = $(doc-dir)/
at-ext = .in

# is this still used?
rpmdocs=BUGS TODO NEWS DEDICATION ANNOUNCE README
rpmdvis=$(rpmmudocs:.doc=.dvi)
rpmexamples= $(addprefix input/, $(notdir $(shell ls $(depth)/input/*.ly)))


