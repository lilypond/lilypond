# empty
TELY_FILES := $(wildcard *.tely)
ITELY_FILES := $(wildcard *.itely)
ITEXI_FILES := $(wildcard *.itexi)

TEXINFO_SOURCES += $(TELY_FILES) $(ITELY_FILES) $(ITEXI_FILES)

EXTRA_DIST_FILES += $(TELY_FILES)

