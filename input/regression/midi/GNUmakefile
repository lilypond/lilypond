depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc midi

TEXI2HTML_FLAGS += --nomenu

TMP_FILES = $(sort $(LY_FILES) $(TEXINFO_SOURCES) )
COLLATED_FILES = ${TMP_FILES:%.ly=$(outdir)/%-midi.ly}

include $(depth)/make/stepmake.make

TITLE=MIDI test suite
