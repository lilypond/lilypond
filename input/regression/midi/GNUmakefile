depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc midi

TEXI2HTML_FLAGS += --nomenu

TMP = $(sort $(LY_FILES) $(TEXINFO_SOURCES) )
COLLATED_FILES = ${TMP:%.ly=$(outdir)/%-midi.ly}

include $(depth)/make/stepmake.make

TITLE=MIDI test suite
