# rules for directories with LilyPond files.

# empty

# huh ? these are for documentation?!
TELY_FILES := $(wildcard *.tely)

OMF_FILES += $(foreach format, html pdf ps.gz, $(foreach f, $(TELY_FILES), $(outdir)/$(f:.tely=.$(format)).omf))

ITELY_FILES := $(wildcard *.itely)
ITEXI_FILES := $(wildcard *.itexi)
LY_FILES := $(wildcard *.ly)
LYINC_FILES := $(wildcard *.lyinc)

TEXINFO_SOURCES += $(TELY_FILES) $(ITELY_FILES) $(ITEXI_FILES)

EXTRA_DIST_FILES += $(TELY_FILES) $(LY_FILES) $(ITEXI_FILES) $(ITELY_FILES) $(LYINC_FILES)

DVIPS_FLAGS= -u+ec-mftrace.map -u +lilypond.map  -Ppdf
