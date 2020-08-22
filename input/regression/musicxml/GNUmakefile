depth = ../../..

STEPMAKE_TEMPLATES=texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc

TEXI2HTML_FLAGS += --nomenu

COLLATED_FILES = $(sort $(MUSICXML_FILES) $(MUSICMXL_FILES) $(TEXINFO_SOURCES) )

MUSICXML_FILES = $(call src-wildcard,*.xml)
MUSICMXL_FILES = $(call src-wildcard,*.mxl)  # Allow .mxl for compressed files
OUT_LY_FILES = $(sort ${MUSICXML_FILES:%.xml=$(outdir)/%.ly} ${MUSICMXL_FILES:%.mxl=$(outdir)/%.ly})
OUT_FILES = $(OUT_LY_FILES)

TITLE=Unofficial MusicXML test suite
AUTHOR=Reinhold Kainhofer
TEST_SUITE_VERSION=0.1


.SUFFIXES: .xml .mxl


include $(depth)/make/stepmake.make

$(outdir)/%.ly:  %.xml
	$(call ly_progress,Making,$@,< xml)
	$(PYTHON) $(MUSICXML2LY) -o $@ $<

$(outdir)/%.ly:  %.mxl
	$(call ly_progress,Making,$@,< mxl)
	$(PYTHON) $(MUSICXML2LY) -z -o $@ $<

ifeq ($(out),www)
local-WWW: zip
endif

zip: $(outdir)/MusicXML-TestSuite-$(TEST_SUITE_VERSION).zip

$(outdir)/MusicXML-TestSuite-$(TEST_SUITE_VERSION).zip: $(MUSICXML_FILES) $(MUSICMXL_FILES) LICENSE
	$(call ly_progress,Making,$@,)
	$(ZIP) -q -r $@ $(addprefix $(src-dir)/,$(MUSICXML_FILES)) $(addprefix $(src-dir)/,$(MUSICMXL_FILES)) LICENSE
