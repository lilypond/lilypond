depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc abc

TEXI2HTML_FLAGS += --nomenu

TMP_FILES = $(sort $(ABC_FILES) $(TEXINFO_SOURCES) )
COLLATED_FILES = ${TMP_FILES:%.abc=$(outdir)/%.ly}


include $(depth)/make/stepmake.make

TITLE=abc2ly test suite
