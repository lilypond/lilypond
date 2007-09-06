# rules for directories with MusicXML files.

# empty

# UGH UGH
include $(make-dir)/lilypond-vars.make

# huh ? these are for documentation?!
MUSICXML_FILES := $(call src-wildcard,*.xml)
# LY_FILES=$(addprefix $(outdir)/, $(addsuffix .ly, $(MUSICXML_FILE)))
# LY_FILES = $(MUSICXML_FILES:%.xml=$(outdir)/%.ly)
LY_FILES = $(MUSICXML_FILES:%.xml=$(outdir)/%.ly)


OUT_FILES = $(LY_FILES)

EXTRA_DIST_FILES +=$(MUSICXML_FILES)
