# rules for directories with MusicXML files.

MUSICXML_FILES = $(call src-wildcard,*.xml)
MUSICMXL_FILES = $(call src-wildcard,*.mxl)  # Allow .mxl for compressed files
OUT_LY_FILES = $(sort ${MUSICXML_FILES:%.xml=$(outdir)/%.ly} ${MUSICMXL_FILES:%.mxl=$(outdir)/%.ly} ${EXTRA_OUT_LY_FILES})
OUT_FILES = $(OUT_LY_FILES)

EXTRA_DIST_FILES += $(MUSICXML_FILES) $(MUSICMXL_FILES)
