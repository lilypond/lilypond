depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc musicxml

TEXI2HTML_FLAGS += --nomenu

COLLATED_FILES = $(sort $(MUSICXML_FILES) $(MUSICMXL_FILES) $(TEXINFO_SOURCES) )


include $(depth)/make/stepmake.make


TITLE=Unofficial MusicXML test suite
AUTHOR=Reinhold Kainhofer
TEST_SUITE_VERSION=0.1
LILYPOND_BOOK_FLAGS += --load-custom-package=$(src-dir)/book-musicxml-testsuite.py
# This breaks since *.tex and *.texi still contains ac/lily-xxxxxx references!
#LILYPOND_BOOK_FLAGS += --use-source-file-names

ifeq ($(out),www)
local-WWW-2: zip
endif

zip: $(outdir)/MusicXML-TestSuite-$(TEST_SUITE_VERSION).zip

$(outdir)/MusicXML-TestSuite-$(TEST_SUITE_VERSION).zip: $(MUSICXML_FILES) $(MUSICMXL_FILES) LICENSE
	$(ZIP) -q -r $@ $(addprefix $(src-dir)/,$(MUSICXML_FILES)) $(addprefix $(src-dir)/,$(MUSICMXL_FILES)) LICENSE
