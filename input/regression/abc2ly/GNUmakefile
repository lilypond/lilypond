depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc abc

TEXI2HTML_FLAGS += --nomenu

# Urgh, how can I do two replacements at once without messing up the order of the list?
TMP = $(sort $(ABC_FILES) $(TEXINFO_SOURCES) )
COLLATED_FILES = ${TMP:%.abc=$(outdir)/%.ly}


include $(depth)/make/stepmake.make

TITLE=abc2ly test suite
