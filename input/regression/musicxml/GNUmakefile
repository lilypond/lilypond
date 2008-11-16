depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc musicxml

TITLE=Lilypond musicxml2ly Regression Tests


OUT_ITEXI_FILES = ${ITEXI_FILES:%.itexi=$(outdir)/%.itexi}

COLLATED_FILES = $(sort $(OUT_LY_FILES) $(OUT_ITEXI_FILES) )


include $(depth)/make/stepmake.make
TITLE=Lilypond musicxml2ly Regression Tests

$(outdir)/%.itexi: %.itexi
	ln -f $< $@
