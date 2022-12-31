depth = ../../..

TEMPLATES = lilypond ly lysdoc texinfo

TEXI2HTML_FLAGS += --nomenu

MUSICXML_FILES = $(call src-wildcard,*.xml)
MUSICMXL_FILES = $(call src-wildcard,*.mxl)  # Allow .mxl for compressed files

COLLATED_FILES = $(sort $(MUSICXML_FILES) $(MUSICMXL_FILES) $(ITEXI_FILES))

TEST_SUITE_VERSION=0.1


.SUFFIXES: .xml .mxl


include $(depth)/make/lilypond.make

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


TITLE=Unofficial MusicXML test suite
AUTHOR=Reinhold Kainhofer
