# rules for directories with ABC files.

ABC_FILES = $(call src-wildcard,*.abc)
OUT_LY_FILES = $(sort ${ABC_FILES:%.abc=$(outdir)/%.ly})
OUT_FILES = $(OUT_LY_FILES)

EXTRA_DIST_FILES += $(ABC_FILES)
