# rules for directories with LilyPond files.

# empty

# huh ? these are for documentation?!
TELY_FILES := $(wildcard *.tely)
ITELY_FILES := $(wildcard *.itely)
ITEXI_FILES := $(wildcard *.itexi)
LY_FILES := $(wildcard *.ly)

TEXINFO_SOURCES += $(TELY_FILES) $(ITELY_FILES) $(ITEXI_FILES)

EXTRA_DIST_FILES += $(TELY_FILES) $(LY_FILES) $(ITEXI_FILES) $(ITELY_FILES)

