depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc musicxml

TEXI2HTML_FLAGS += --nomenu

COLLATED_FILES = $(sort $(MUSICXML_FILES) $(MUSICMXL_FILES) $(TEXINFO_SOURCES) )


include $(depth)/make/stepmake.make


TITLE=Unofficial MusicXML test suite
AUTHOR=Reinhold Kainhofer
TEST_SUITE_VERSION=0.1

ifeq ($(out),www)
local-WWW-2: zip
endif

zip: $(outdir)/MusicXML-TestSuite-$(TEST_SUITE_VERSION).zip

$(outdir)/MusicXML-TestSuite-$(TEST_SUITE_VERSION).zip: $(MUSICXML_FILES) $(MUSICMXL_FILES) LICENSE
	$(call ly_progress,Making,$@,)
	$(ZIP) -q -r $@ $(addprefix $(src-dir)/,$(MUSICXML_FILES)) $(addprefix $(src-dir)/,$(MUSICMXL_FILES)) LICENSE
