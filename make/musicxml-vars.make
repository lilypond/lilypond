# rules for directories with MusicXML files.

MUSICXML_FILES := $(call src-wildcard,*.xml)
# LY_FILES=$(addprefix $(outdir)/, $(addsuffix .ly, $(MUSICXML_FILE)))
# LY_FILES = $(MUSICXML_FILES:%.xml=$(outdir)/%.ly)
OUT_LY_FILES = $(MUSICXML_FILES:%.xml=$(outdir)/%.ly)

OUT_FILES = $(OUT_LY_FILES)

EXTRA_DIST_FILES += $(MUSICXML_FILES)
