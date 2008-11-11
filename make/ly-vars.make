# rules for directories with LilyPond files.

# empty

# huh ? these are for documentation?!
TELY_FILES := $(call src-wildcard,*.tely)
MASTER_TEXI_FILES := $(sort $(TELY_FILES:%.tely=$(outdir)/%.texi))\
 $(OUT_MASTER_TEXI_FILES)

OMF_FILES += $(foreach format, html pdf, $(foreach f, $(TELY_FILES), $(outdir)/$(f:.tely=.$(format)).omf))

ITELY_FILES := $(call src-wildcard,*.itely)
ITEXI_FILES := $(call src-wildcard,*.itexi)
LY_FILES := $(call src-wildcard,*.ly)
ILY_FILES := $(call src-wildcard,*.ily)

TEXINFO_SOURCES += $(TELY_FILES) $(ITELY_FILES) $(ITEXI_FILES)

EXTRA_DIST_FILES +=$(TELY_FILES) $(LY_FILES) $(ITEXI_FILES) $(ITELY_FILES) $(ILY_FILES)

