
# override Generic_vars.make:
DIST_FILES := $(EXTRA_DIST_FILES) 

TO_TOP_FILES=$(addprefix $(outdir)/, $(README_TOP_FILES))

# urg?
include $(stepdir)/documentation-vars.make

