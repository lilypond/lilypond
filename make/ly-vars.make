# rules for directories with LilyPond files.

# empty

# huh ? these are for documentation?!
TELY_FILES := $(wildcard *.tely)

OMF_FILES += $(foreach format, html pdf ps.gz, $(foreach f, $(TELY_FILES), $(outdir)/$(f:.tely=.$(format)).omf))

ITELY_FILES := $(wildcard *.itely)
ITEXI_FILES := $(wildcard *.itexi)
LY_FILES := $(wildcard *.ly)
ILY_FILES := $(wildcard *.ily)

TEXINFO_SOURCES += $(TELY_FILES) $(ITELY_FILES) $(ITEXI_FILES)

EXTRA_DIST_FILES +=$(TELY_FILES) $(LY_FILES) $(ITEXI_FILES) $(ITELY_FILES) $(ILY_FILES)


# not mf/out , not mf/$(outdir) 
DVIPS_FLAGS= -u+$(builddir)/mf/out/lilypond.map  -Ppdf
