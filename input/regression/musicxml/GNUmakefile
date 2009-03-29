depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc musicxml

TEXI2HTML_FLAGS += --nomenu

# Urgh, how can I do two replacements at once without messing up the order of the list?
TMP = $(sort $(MUSICXML_FILES) $(MUSICMXL_FILES) $(TEXINFO_SOURCES) )
TMP1 = ${TMP:%.xml=$(outdir)/%.ly}
COLLATED_FILES = ${TMP1:%.mxl=$(outdir)/%.ly}


include $(depth)/make/stepmake.make


TITLE=Unofficial MusicXML test suite
TEST_SUITE_VERSION=0.1

local-WWW-2: zip

zip: $(outdir)/MusicXML-TestSuite-$(TEST_SUITE_VERSION).zip

$(outdir)/MusicXML-TestSuite-$(TEST_SUITE_VERSION).zip: $(MUSICXML_FILES) $(MUSICMXL_FILES)
	$(ZIP) -r $@ $(addprefix $(src-dir)/,$(MUSICXML_FILES)) $(addprefix $(src-dir)/,$(MUSICMXL_FILES))
