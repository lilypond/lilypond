# stepmake/Makedir.shared.make
# urg: stuff shared between yodl/lily/stepmake

LSM_FILES:= $(wildcard *.lsm.in)
OUTLSM_FILES=$(addprefix $(outdir)/,$(basename $(LSM_FILES), .in))
SPEC_FILES:= $(wildcard *.spec.in)
OUTSPEC_FILES=$(addprefix $(outdir)/,$(basename $(SPEC_FILES), .in))

EXTRA_DIST_FILES = $(state-vector)

# these two outdir FILES are distributed, since they make sense to have 
# without running configure and make.
OUT_DIST_FILES= $(OUTLSM_FILES) $(OUTSPEC_FILES)


spec: $(OUTSPEC_FILES)

$(OUTSPEC_FILES): $(depth)/VERSION 
$(OUTLSM_FILES): $(depth)/VERSION

AT_FILES = $(BLURBS) # 
at-dir = $(doc-dir)/
at-ext = .in

# is this still used?
rpmdocs=BUGS TODO NEWS DEDICATION ANNOUNCE README
rpmdvis=$(rpmmudocs:.doc=.dvi)
rpmexamples= $(addprefix input/, $(notdir $(shell ls $(depth)/input/*.ly)))


localdist:   $(OUTSPEC_FILES) $(OUTLSM_FILES)
