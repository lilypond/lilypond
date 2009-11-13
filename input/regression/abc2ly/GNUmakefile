depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc abc

TEXI2HTML_FLAGS += --nomenu

TMP = $(sort $(ABC_FILES) $(TEXINFO_SOURCES) )
COLLATED_FILES = ${TMP:%.abc=$(outdir)/%.ly}


include $(depth)/make/stepmake.make

TITLE=abc2ly test suite
