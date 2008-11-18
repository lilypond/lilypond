depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc musicxml

TITLE=Lilypond musicxml2ly Regression Tests


# Urgh, how can I do two replacements at once without messing up the order of the list?
TMP = $(sort $(MUSICXML_FILES) $(MUSICMXL_FILES) $(TEXINFO_SOURCES) )
TMP1 = ${TMP:%.xml=$(outdir)/%.ly}
COLLATED_FILES = ${TMP1:%.mxl=$(outdir)/%.ly}


include $(depth)/make/stepmake.make
TITLE=Lilypond musicxml2ly Regression Tests
